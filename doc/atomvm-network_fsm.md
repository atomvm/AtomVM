# AtomVM Network FSM Programming Manual

The AtomVM `network_fsm` provides an Erlang API interface for interacting with the networking layer of the device on which AtomVM is running.

This API is designed to allow applications to be responsive to changes in the underlying network, which can frequently occur in embedded applications, where devices can easily lose and then regain network connectivity.  In such cases, it is important for applications to be resilient to changes in network avaialbility, by closing or re-opening socket connections in response to disconnections and re-connections in the underlying network.

This document describes the basic design of the AtomVM Network FSM, and how to interact programmatically with it.

## Finite State Machine

The Network FSM implemented as a finite state machine, which in turn is an idealized representation of the underlying behavior of the underlying network layer that is instantiated on the device on which AtomVM is running.

When the Network FSM is started, it initializes the underlying network layer and transitions into the `wait_for_connection` state, waiting for a message from the network layer that it has connected to the network.  Once the Network FSM receives a `connected` message, it transitions to the `wait_for_ip` state, where it will wait for a message from the underlying network layer, telling it that it has received an IP address.  Once it receives a `{got_ip, {Address, Netmask, Gateway}}` message, containing the `Address`, `Netmask`, and `Gateway` information of the obtained IP address, it transitions into the `got_ip` state.

When the Network FSM is in the `wait_for_ip` or `got_ip` state, it may also receive a `disconnected` message, which will transition the FSM into the `wait_for_connection` state.

In addition, if the Network FSM is in any of the `wait_for_connection`, `wait_for_ip`, or `got_ip` states, the FSM may be asked to stop by the application.  In that case, the Network FSM will terminate.

The following diagram illustrates the Network FSM states, messages, and state transitions.

                         init
                           |
                           v
                +---------------------+
        +-------| wait_for_connection | <-------+
        |       +---------------------+         |
        |                  |                    |
        | stop             | connected          | disconnected
        |                  v                    |
        |       +---------------------+         |
        +-------|     wait_for_ip     |---------+
        |       +---------------------+         |
        |                  |                    |
        |                  | {got_ip, {Address, Netmask, Gateway}}
        |                  v                    |
        |       +---------------------+         |
        +-------|       got_ip        |---------+
        |       +---------------------+
        |
        v
       halt

> Note.  Stopping a Network FSM is currently not well supported.

### Network Callbacks

As part of the above state transitions, the calling application may register callback functions which will be invoked during state transitions.

> Note. The state in which the Network FSM is in at the point at which a callback function is called is undefined and subject to change without notification.  Applications should only use information passed in as parameters as inputs , as well as perhaps contextual information provided through a closure, to determine the behavior of the callback.

The following callbacks/arities may be specified:

* `connected/0`:  Invoked when the Network FSM receives a `connected` message.  No arguments are supplied to the callback.
* `disconnected/0`:  Invoked when the Network FSM receives a `disconnected` message.  No arguments are supplied to the callback.
* `got_ip/1`: Invoked when the Network FSM receives a `{got_ip, {Address, Netmask, Gateway}}` message.  The `{Address, Netmask, Gateway}` tuple is supplied as the single argument to the specified callback.

Applications may choose not to specify a callback function for any given state transition.

## Network FSM API

The AtomVM Network FSM is driven by a programmatically simple API.  The behavior of the FSM is largely dictated by the configuration used to set up the FSM, as described below.

### Starting the Network FSM

Starting a Network FSM instance is perfomed via the `start/1` operation, which takes as a single argument the configuration to be used:

    Config = ...
    ok = network_fsm:start(Config).
    ...

> Note.  You can only have one instance of the Network FSM running at a time.

The structure of the configuration parameter is described below.

### Configuration

The configuration of the Network FSM is goverened by a nested properties list, representing the different modes in which the Network FSM is to be instantiated.

The confgiguration has the following type specification:

    -type mode_config() :: sta_config().
    -type network_config() :: [mode_config()].

> Note. Currently, the Network FSM only supports station mode configuration.

> TODO Add support for SoftAP mode.

#### Station Mode configuration

Station mode configuration is used to configure the device in station mode (i.e, connected to an access point, such as a router or hotspot device).

Use the `sta` tag to denote this mode:

    -type sta_config() :: {sta, [sta_config_property()]}.

A `sta_config_property()` is a properties list whose types are defined as follows:

    -type ssid_config() :: {ssid, binary()}.
    -type psk_config() :: {psk, binary()}.
    -type connected_config() :: {connected, fun(() -> term())}.
    -type disconnected_config() :: {disconnected, fun(() -> term())}.
    -type got_ip_config() :: {got_ip, fun((ip_info()) -> term())}.
    -type sta_config_property() :: ssid_config() | psk_config() | connected_config() | disconnected_config() | got_ip_config().

Notes:

* The `ssid` parameter denotes the network id and is _required_ for WIFI networks.  However, the STA SSID (and PSK) _may_ be stored in NVS storage (see below).
* The `psk` parameter denotes the password or phrase used to authenticate to the network.  This parameter is not required on open networks (strongly discouraged)
* The `connected_config()`, `disconnected_config()`, and `got_ip_config()` parameters denote callbacks that get called during Network FSM state transitions descibed above.  These paramters are optional, but use of them is strongly encouraged, in order to design robust applications.

The `got_ip_config()` callback function takes a `ip_info()` structure, whose type is described as follows:

    -type octet() :: 0..255.
    -type ipv4_address() :: {octet(), octet(), octet(), octet()}.
    -type ipv4_info() :: {IPAddress::ipv4_address(), NetMask::ipv4_address(), Gateway::ipv4_address()}.
    -type ip_info() :: ipv4_info().

> Note.  Currently, only IPv4 addresses are supported.

#### NVS Credentials

You may store a STA SSID and PSK in non-volatile storage (NVS) on and ESP32 device under the `atomvm` namespace.  The `network.hrl` header file defines macros for the network NVS namespace, and for the STA SSID and PSK keys.

If set in NVS storage, you may remove the `ssid` and `psk` parameters from the `ssid_config()` used to initialize the network, and the SSID and PSK configured in NVS will be used, instead.  An SSID or PSK defined in configuration will override any values in NVS.

You can set these credentials once, as follows:

    esp:nvs_get_binary(?ATOMVM_NVS_STA_SSID, <<"myssid">>).
    esp:nvs_get_binary(?ATOMVM_NVS_STA_PSK, <<"mypsk">>).

where `MySSID` and `MyPSK` are the SSID and password, respectively, for your network.  A sample program is provided, which can be edited with your SSID and PSK, flashed, and run once.  From that point forward (until you reformat NVS storage, or delete the SSID and PSK entries), you can run ESP prorgams that initialize the network without configuring your SSID and PSK for your local network.

> Note.  Credentials are stored unencrypted and in plaintext and should not be considered secure.  Future versions may use encrypted NVS storage.

### Stopping the Network FSM

To stop the Network FSM, issue the `stop/0` function:

    network_fsm:stop().

> Note.  This function is currently not well tested.

# Example

The following example connects to a network ("myssid") using a password ("mypsk"), and handles `connected`, `disconnected`, and `got_ip` events by displying them to the console.

    -module(sta_network).

    -export([start/0]).

    start() ->
        Self = self(),
        Config = [
            {sta, [
                {ssid, "myssid"},
                {psk,  "mypsk"},
                {connected, fun() -> Self ! connected end},
                {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
                {disconnected, fun() -> Self ! disconnected end}
            ]}
        ],
        case network_fsm:start(Config) of
            ok ->
                wait_for_message();
            Error ->
                erlang:display(Error)
        end.

    wait_for_message() ->
        receive
            connected ->
                erlang:display(connected);
            {ok, IpInfo} ->
                erlang:display(IpInfo);
            disconnected ->
                erlang:display(disconnected)
        end,
        wait_for_message().
