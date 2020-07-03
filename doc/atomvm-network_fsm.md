# AtomVM Network Programming Manual

One of the exciting features of the ESP32 is its support for WIFI networking, allowing ESP32 micro-controllers to communicate with the outside world.  The ESP32 and ISF SDK supports configuring an ESP32 in station mode (STA), whereby the device connects to an existing accees point, as well as "softAP" mode (AP), whereby it functions as an access point, to which other stations can connect.   The ESP32 also supports a combined STA+softAP mode, which allows the device to function in both STA and softAP mode.

AtomVM provides an Erlang API interface for interacting with the WIFI networking layer on ESP32 devices, providing support for configuring your ESP32 device in STA or AP mode, allowing Erlang/Elixir applications to send and receive data from other devices on a network.  This interface is encapsulated in the `network_fsm` module, which implements a simple state machine for connecting to existing WIFI networks or for functioning as a WIFI access point.

Once the network has been set up (in STA or AP mode), AtomVM can use the `gen_tcp` and `gen_udp` interfaces from the `estdlib` AtomVM library to interact with the socket layer to create a client or server application.

The AtomVM networking API is designed to allow applications to be responsive to changes in the underlying network, which can frequently occur in embedded applications, where devices can easily lose and then regain network connectivity.  In such cases, it is important for applications to be resilient to changes in network avaialbility, by closing or re-opening socket connections in response to disconnections and re-connections in the underlying network.

This document describes the basic design of the AtomVM network interfaces, and how to interact programmatically with it.

## Getting Started

The `examples/erlang/esp32/sta_network.erl` and `examples/erlang/esp32/ap_network.erl` sample programs provide examples of how to connect to an existing WIFI network,

In both cases, the WIFI network is initialized using the `network_fsm:start/1` function, which will instantiate the AtomVM network state machine, depending on the configuration.

The input parameter to this function is a nested properties list structure, which may contain configuration for the device in STA mode, as well as configuration for the device in AP mode.  The requirements for this input data structure are described in more detail below, depending on the mode in which the device is being configured.

> Note.  AtomVM does not yet support configuration in `STA+softAP` mode.

### Station (STA) mode

In STA mode, the ESP32 connects to an existing WIFI network.

In this case, the input configuration should be a properties list containing a tuple of the form `{sta, <sta-properties>}`, where `<sta-properties>` is a properties list containing configuration properties for the device in station mode.

The `<sta-properties>` should contain the following entries:

* `{ssid, string() | binary()}`  The SSID to which the device should connect.  This property is required.
* `{psk, string() | binary()}` The password required to authenticate to the network, if required.

> Note that the station mode SSID and password _may_ be stored in non-volatile storage, in which case these parameters may be skipped.  See the "NVS Credentials" section below, for more information about using non-volatile storage to store credentials that persist across device reboots.

The `network_fsm:start/1` will immediately return `ok`, if the network was properly initialized, or `{error, Reason}`, if there was an error in configuration.  However, the application may want to wait for the device to connect to the target network and obtain an IP address, for example, before starting clients or services that require network access.

Applications can specify callback functions, which get triggered as events emerge from the network layer, including connection to and disconnection from the target network, as well as IP address acquisition.

Callback functions can be specified by the following configuration parameters:

* `{connected, fun(() -> term())}` A callback function which will be called when the device connects to the target network.
* `{disconnected, fun(() -> term())}` A callback function which will be called when the device disconnects from the target network.
* `{got_ip, fun((ip_info()) -> term())}` A callback function which will be called when the device obtains an IP address.  In this case, the IPv4 IP address, netmask, and gateway are provided as a parameter to the callback function.

> Note.  IPv6 addresses are not yet supported in AtomVM.

Callback functions are completely optional, but are highly recommended for building robust WIFI applications.  The return value from callback functions is ignored.

In addition, the following optional parameters can be specified to configure the AP network:

* `{dhcp_hostname, string()|binary()}` The DHCP hostname as which the device should register (`<<"atomvm-<hexmac>">>`, where `<hexmac>` is the hexadecimal representation of the factory-assigned MAC address of the device , by default)

The following example illustrates initialization of the WIFI network in STA mode.  The example program will configure the network to connect to a specified network, and then wait to receive events from the underlying network.

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

In a typical application, the network should be configured and an IP address should be acquired first, before starting clients or services that have a dependency on the network.

### AP mode

In AP mode, the ESP32 starts a WIFI network to which other devices (laptops, mobile devices, other ESP32 devices, etc) can connect.  The ESP32 will create an IPv4 network, and will assign itself the address `192.168.4.1`.  Devices that attach to the ESP32 in AP mode will be assigned sequential addresses in the `192.168.4.0/24` range, e.g., `192.168.4.2`, `192.168.4.3`, etc.

To intialize the ESP32 device in AP mode, the input configuration should be a properties list containing a tuple of the form `{ap, <ap-properties>}`, where `<ap-properties>` is a properties list containing configuration properties for the device in AP mode.

The `<ap-properties>` may contain the following entries:

* `{ssid, string() | binary()}`  The SSID to which the device should connect.
* `{psk, string() | binary()}` The password required to authenticate to the network, if required.  Note that this password must be a minimum of 8 characters.

If the SSID is ommited in configuration, the SSID name `atomvm-<hexmac>` will be created, where `<hexmac>` is the hexadecimal representation of the factory-assigned MAC address of the device.  This name should be sufficiently unique to disabiguate it from other reachable ESP32 devices, but it may also be difficult to read or remember.

If the password is omitted, then an _open network_ will be created, and a warning will be printed to the console.  Otherwise, the AP nework will be started using WPA+WPA2 authentication.

> Note that the station mode SSID and password _may_ be stored in non-volatile storage, in which case these parameters may be skipped.  See the "NVS Credentials" section below, for more information about using non-volatile storage to store credentials that persist across device reboots.

The `network_fsm:start/1` will immediately return `ok`, if the network was properly initialized, or `{error, Reason}`, if there was an error in configuration.  However, the application may want to wait for the device to to be ready to accept connections from other devices, or to be notified when other devices connect to this AP.

Applications can specify callback functions, which get triggered as events emerge from the network layer, including when a station connects or disconnects from the AP, as well as when a station is assigned an IP address.

Callback functions can be specified by the following configuration parameters:

* `{ap_started, fun(() -> term())}` A callback function which will be called when the AP endpoint has started and is ready to be connected to.
* `{sta_connected, fun((Mac::binary()) -> term())}` A callback function which will be called when a device connects to the AP.  The MAC address of the station, as a 6-byte binary, is passed to the callback function.
* `{sta_disconnected, fun((Mac::binary()) -> term())}` A callback function which will be called when a device disconnects from the AP.  The MAC address of the station, as a 6-byte binary, is passed to the callback function.
* `{sta_ip_assigned, fun((ipv4_address()) -> term())}` A callback function which will be called when the AP assigns an IP address to a station.  The assigned IP address is passed to the callback function.

> Note.  IPv6 addresses are not yet supported in AtomVM.

Callback functions are completely optional, but are highly recommended for building robust WIFI applications.  The return value from callback functions is ignored.

In addition, the following optional parameters can be specified to configure the AP network:

* `{ssid_hidden, boolean()}` Whether the AP network should be not be broadcast (false, by default)
* `{max_connections, non_neg_integer()}` The maximum number of devices that can connect to this network (by default, 4)

The following example illustrates initialization of the WIFI network in AP mode.  The example program will configure the network to connect to start a WIFI network with the name `myssid` and password `!!MVmota`, and then wait to receive events from the underlying network.

    -module(ap_network).

    -export([start/0]).

    start() ->
        Config = [
            {ap, [
                {ssid, esp:nvs_get_binary(atomvm, ap_ssid, <<"myssid">>)},
                {psk,  esp:nvs_get_binary(atomvm, ap_psk, <<"mypsk">>)},
                {ap_started, fun ap_started/0},
                {sta_connected, fun ap_sta_connected/1},
                {sta_ip_assigned, fun ap_sta_ip_assigned/1},
                {sta_disconnected, fun ap_sta_disconnected/1}
            ]}
        ],
        case network_fsm:start(Config) of
            ok ->
                sleep_forever();
            Error ->
                erlang:display(Error)
        end.

    ap_started() ->
        io:format("AP started.~n").

    ap_sta_connected(Mac) ->
        io:format("STA connected with mac ~p~n", [Mac]).

    ap_sta_disconnected(Mac) ->
        io:format("STA disconnected with mac ~p~n", [Mac]).

    ap_sta_ip_assigned(Address) ->
        io:format("STA assigned address ~p~n", [Address]).

    sleep_forever() ->
        timer:sleep(10000),
        sleep_forever().

In a typical application, the network should be configured and the application should wait for the AP to report that it has started, before starting clients or services that have a dependency on the network.

## SNTP Support

You may configure the networking layer to automatically synchronize time on the ESP32 with an NTP server accessible on the network.

To synchronize time with an NTP server, add a property with the tag `sntp` at the top level configuration passed into the `network_fsm:start/1` function.  Specify the NTP hostname or IP address with which your device should sync.

* `{sntp, string() | binary()}`  The NTP host with which the device should sync time.

## NVS Credentials

It can become tiresome to enter an SSID and password for every application, and in general it is bad security practice to hard-wire WIFI credentials in your application source code.

You may instead store an STA or AP SSID and PSK in non-volatile storage (NVS) on and ESP32 device under the `atomvm` namespace.  The `network.hrl` header file defines macros for the network NVS namespace, and for the STA SSID and PSK keys.

If set in NVS storage, you may remove the `ssid` and `psk` parameters from the configuration used to initialize the network, and the SSID and PSK configured in NVS will be used, instead.  However, an SSID or PSK defined explicitly in configuration will override any values in NVS.

You can set these credentials once, as follows:

    esp:nvs_set_binary(?ATOMVM_NVS_STA_SSID, <<"myssid">>).
    esp:nvs_set_binary(?ATOMVM_NVS_STA_PSK, <<"mypsk">>).

or

    esp:nvs_set_binary(?ATOMVM_NVS_AP_SSID, <<"myssid">>).
    esp:nvs_set_binary(?ATOMVM_NVS_AP_PSK, <<"mypsk">>).

where `MySSID` and `MyPSK` are the SSID and password, respectively, for your network.  A sample program is provided, which can be edited with your SSID and PSK, flashed, and run once.  From that point forward (until you reformat NVS storage, or delete the SSID and PSK entries), you can run ESP prorgams that initialize the network without configuring your SSID and PSK for your local network.

> Note.  Credentials are stored unencrypted and in plaintext and should not be considered secure.  Future versions may use encrypted NVS storage.

## Stopping the Network FSM

To stop the Network FSM, issue the `stop/0` function:

    network_fsm:stop().

> Note.  This function is currently not well tested.
