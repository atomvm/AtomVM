<!--
 Copyright 2020-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Network Programming Guide

One of the exciting features of the ESP32 and the Pico-W is their support for WiFi networking, allowing ESP32 and Pico-W micro-controllers to communicate with the outside world over common IP networking protocols, such as TCP or IDP.  The ESP32 and the Pico-W can be configured in station mode (STA), whereby the devices connect to an existing access point, as well as "softAP" mode (AP), whereby they function as an access point, to which other stations can connect. The ESP32 also supports a combined STA+softAP mode, which allows the device to function in both STA and softAP mode simultaneously.

AtomVM provides an Erlang API interface for interacting with the WiFi networking layer on ESP32 and Pico-W devices, providing support for configuring your ESP32 or Pico-W device in STA mode, AP mode, or a combined STA+AP mode, allowing Erlang/Elixir applications to send and receive data from other devices on a network.  This interface is encapsulated in the [`network` module](./apidocs/erlang/eavmlib/network.md), which implements a simple interface for connecting to existing WiFi networks or for functioning as a WiFi access point. The same `network` module is used for both the ESP32 and the Pico-W.

Once the network has been set up (in STA or AP mode), AtomVM can use various socket interfaces to interact with the socket layer to create a client or server application.  For example, on ESP32, AtomVM supports the [`gen_udp`](./apidocs/erlang/estdlib/gen_udp.md) and [`gen_tcp`](./apidocs/erlang/estdlib/gen_tcp.md) APIs, while AtomVM extensions may support HTTP, MQTT, and other protocols built over low-level networking interfaces.

The AtomVM networking API leverages callback functions, allowing applications to be responsive to changes in the underlying network, which can frequently occur in embedded applications, where devices can easily lose and then regain network connectivity.  In such cases, it is important for applications to be resilient to changes in network availability, by closing or re-opening socket connections in response to disconnections and re-connections in the underlying network.

This document describes the basic design of the AtomVM network interfaces, and how to interact programmatically with it.

## Station (STA) mode

In STA mode, the ESP32 or the Pico-W connect to an existing WiFi network.

In this case, the input configuration should be a properties list containing a tuple of the form `{sta, <sta-properties>}`, where `<sta-properties>` is a property list containing configuration properties for the device in station mode.

The `<sta-properties>` property list should contain the following entries:

* `{ssid, string() | binary()}`  The SSID to which the device should connect.
* `{psk, string() | binary()}` The password required to authenticate to the network, if required.

The [`network:start/1`](./apidocs/erlang/eavmlib/network.md#start1) will immediately return `{ok, Pid}`, where `Pid` is the process ID of the network server instance, if the network was properly initialized, or `{error, Reason}`, if there was an error in configuration.  However, the application may want to wait for the device to connect to the target network and obtain an IP address, for example, before starting clients or services that require network access.

Applications can specify callback functions, which get triggered as events emerge from the network layer, including connection to and disconnection from the target network, as well as IP address acquisition.

Callback functions can be specified by the following configuration parameters:

* `{connected, fun(() -> term())}` A callback function which will be called when the device connects to the target network.
* `{disconnected, fun(() -> term())}` A callback function which will be called when the device disconnects from the target network.
* `{got_ip, fun((ip_info()) -> term())}` A callback function which will be called when the device obtains an IP address.  In this case, the IPv4 IP address, net mask, and gateway are provided as a parameter to the callback function.

```{warning}
IPv6 addresses are not yet supported in AtomVM.
```

Callback functions are optional, but are highly recommended for building robust WiFi applications.  The return value from callback functions is ignored, and AtomVM provides no guarantees about the execution context (i.e., BEAM process) in which these functions are invoked.

In addition, the following optional parameters can be specified to configure the AP network (ESP32 only):

* `{dhcp_hostname, string()|binary()}` The DHCP hostname as which the device should register (`<<"atomvm-<hexmac>">>`, where `<hexmac>` is the hexadecimal representation of the factory-assigned MAC address of the device).
* `{beacon_timeout, fun(() -> term())}` A callback function which will be called when the device does not receive a beacon frame from the connected access point during the "inactive time" (6 second default, currently not configurable).

The following example illustrates initialization of the WiFi network in STA mode.  The example program will configure the network to connect to a specified network.  Events that occur during the lifecycle of the network will trigger invocations of the specified callback functions.

```erlang
Config = [
    {sta, [
        {ssid, <<"myssid">>},
        {psk,  <<"mypsk">>},
        {connected, fun connected/0},
        {got_ip, fun got_ip/1},
        {disconnected, fun disconnected/0}
        {dhcp_hostname, <<"myesp32">>}
    ]}
],
{ok, Pid} = network:start(Config),
...
```

The following callback functions will be called when the corresponding events occur during the lifetime of the network connection.

```erlang
connected() ->
    io:format("Connected to AP.~n").

gotIp(IpInfo) ->
    io:format("Got IP: ~p~n", [IpInfo]).

disconnected() ->
    io:format("Disconnected from AP, starting scan~n"),
    {ok, {Num, NetworkList}} = network:wifi_scan(),
    ...
```

In a typical application, the network should be configured and an IP address should be acquired first, before starting clients or services that have a dependency on the network.

### STA Mode Convenience Functions

The `network` module supports the [`network:wait_for_sta/1,2`](./apidocs/erlang/eavmlib/network.md#wait_for_sta1) convenience functions for applications that do not need robust connection management.  These functions are synchronous and will wait until the device is connected to the specified AP.  Supply the properties list specified in the `{sta, [...]}` component of the above configuration, in addition to an optional timeout (in milliseconds).

For example:

```erlang
Config = [
    {ssid, <<"myssid">>},
    {psk,  <<"mypsk">>},
    {dhcp_hostname, <<"mydevice">>}
],
case network:wait_for_sta(Config, 15000) of
    {ok, {Address, _Netmask, _Gateway}} ->
        io:format("Acquired IP address: ~p~n", [Address]);
    {error, Reason} ->
        io:format("Network initialization failed: ~p~n", [Reason])
end
```

### STA (or AP+STA) mode functions

Some functions are only available if the device is configured in STA or AP+STA mode.

#### `sta_rssi`

Once connected to an access point, the signal strength in decibel-milliwatts (dBm) of the connection to the associated access point may be obtained using [`network:sta_rssi/0`](./apidocs/erlang/eavmlib/network.md#sta_rssi0). The value returned as `{ok, Value}` will typically be a negative number, but in the presence of a powerful signal this can be a positive number. A level of 0 dBm corresponds to the power of 1 milliwatt. A 10 dBm decrease in level is equivalent to a ten-fold decrease in signal power.

#### `wifi_scan`

```{notice}
This function is currently only supported on the ESP32 platform.
```

After the network has been configured for STA mode and started, as long as no connection has been initiated or associated, you may scan for available access points using [`network:wifi_scan/0`](./apidocs/erlang/eavmlib/network.md#wifi_scan1) or [`network:wifi_scan/1`](./apidocs/erlang/eavmlib/network.md#wifi_scan1).  Scanning for access points will temporarily inhibit other traffic on the access point network if it is in use, but should not cause any active connections to be dropped.  With no options, a default 'active' scan, with a per-channel dwell time of 120ms will be used and will return network details for up to 6 access points.  The return value for the scan takes the form of a tuple consisting of `{ok, Results}`, where `Results = {FoundAPs [NetworkList]}`. `FoundAPs` may be a number larger than the length of the NetworkList if more access points were discoverd than the number of results requested.  The entries in the `NetworkList` take the form of `{SSID, [AP_Properties]}`. `SSID` is the name of the network, and the `AP_Properties` is a proplist with the keys `rssi` for the dBm signal strength of the access point, `authmode` value is the authentication method used by the network, `bssid` (a.k.a MAC address) of the access point, and the `channel` key for obtaining the primary channel for the network.

Example return results:
```erlang
{ok, {Num, Networks}} = network:wifi_scan(Config),
io:format("network scan found ~p networks.~n", [Num]),
lists:foreach(
    fun(_Network = {SSID, [{rssi, DBm}, {authmode, Mode}, {bssid, BSSID}, {channel, Number}]}) ->
        io:format(
            "Network: ~p, BSSID: ~p, signal ~p dBm, Security: ~p, channel ~p~n",
            [SSID, BSSID, DBm, Mode, Number]
        )
    end,
    Networks
).
```

```{tip}
The bssid binary results can be conveniently formatted for logging using `binary:encode_hex/1`.
```

Due to network stack size limitations on the esp32 classic the maximum number of scan results that can be returned is 14. Other ESP32 family chips can return up to 20 networks in the scan results. The default scan is quite fast, and likely may not find all the available networks.  Scans are quite configurable with `active` (the default) and `passive` modes. Options should take the form of a proplist.  The per channel scan time can be changed with the `dwell` key, the channel dwell time can be set for up to 1500 ms.  Passive scans are slower, as they always linger on each channel for the full dwell time. Passive mode can be used by simply adding `passive` to the configuration proplist. Keep in mind when choosing a dwell time that between each progressively scanned channel the device must return to the home channel for a short time (typically 30ms), but for scans with a dwell time of over 1000ms the home channel dwell time will increase to 60ms to help mitigate beacon timeout events.  In some network configuration beacon timeout events may still occur, but should not lead to a dropped connection, and after the scan completes the device should receive the next beacon from the access point. The default of 6 access points in the returned `NetworkList` may be changed with the `results` key. By default hidden networks are ignored, but can be included in the results by adding `show_hidden` to the configuration.

For example to do a passive scan using and ESP32-C6, including hidden networks, using the longest allowed scan time and showing the maximum number of networks available use the following:

```erlang
{ok, Results} = network:wifi_scan([passive, {results, 20}, {dwell, 1500}, show_hidden]),
```

For convenience the default options used for `network:wifi_scan/0` may be configured along with the `sta_config()` used to start the network driver.  For the corresponding configuration keys consult the [`network:scan_options()`](./apidocs/erlang/eavmlib/network.md#scan-options) type definition. You may also perform a scan without starting the network with a configuration. This will use the configuration `[{sta, [managed]}]`, which uses the default `disconnected` event callback that will always attempt to reconnect to the last network. This makes future use of `network:wifi_scan/0,1` impossible, and `network:disconnect/0` useless as the driver will immediately attempt to reconnect to the last network. This is mainly intended for quick testing purposes. For most applications that will use wifi scan results it is recommended to start the driver with a configuration that uses a custom callback function for `disconnected` events, so that the driver will remain idle and allow the use of scan results to decide if a connection should be made.

## AP mode

In AP mode, the ESP32 starts a WiFi network to which other devices (laptops, mobile devices, other ESP32 devices, etc) can connect.  The ESP32 will create an IPv4 network, and will assign itself the address `192.168.4.1`.  Devices that attach to the ESP32 in AP mode will be assigned sequential addresses in the `192.168.4.0/24` range, e.g., `192.168.4.2`, `192.168.4.3`, etc.

To initialize the ESP32 device in AP mode, the input configuration should be a properties list containing a tuple of the form `{ap, <ap-properties>}`, where `<ap-properties>` is a property list containing configuration properties for the device in AP mode.

The `<ap-properties>` property list may contain the following entries:

* `{ssid, string() | binary()}`  The SSID to which the device should connect.
* `{psk, string() | binary()}` The password required to authenticate to the network, if required.  Note that this password must be a minimum of 8 characters.
* `{ap_channel, wifi_channel()}` The channel the access point should use.

If the SSID is omitted in configuration, the SSID name `atomvm-<hexmac>` will be created, where `<hexmac>` is the hexadecimal representation of the factory-assigned MAC address of the device.  This name should be sufficiently unique to disambiguate it from other reachable ESP32 devices, but it may also be difficult to read or remember.

If the password is omitted, then an __open network__ will be created, and a warning will be printed to the console.  Otherwise, the AP network will be started using WPA+WPA2 authentication.

If the channel is omitted the default chanel for esp32 is `1`. This setting is only used while a device is operation is AP mode only. If `ap_channel` is configured, it will be temporarily changed to match the associated access point if AP + STA mode is used and the station is associated with an access point. This is a hardware limitation due to the modem radio only being able to operate on a single channel (frequency) at a time.

The [`network:start/1`](./apidocs/erlang/eavmlib/network.md#start1) will immediately return `{ok, Pid}`, where `Pid` is the process id of the network server, if the network was properly initialized, or `{error, Reason}`, if there was an error in configuration.  However, the application may want to wait for the device to to be ready to accept connections from other devices, or to be notified when other devices connect to this AP.

Applications can specify callback functions, which get triggered as events emerge from the network layer, including when a station connects or disconnects from the AP, as well as when a station is assigned an IP address.

Callback functions can be specified by the following configuration parameters:

* `{ap_started, fun(() -> term())}` A callback function which will be called when the AP endpoint has started and is ready to be connected to.
* `{sta_connected, fun((Mac::binary()) -> term())}` A callback function which will be called when a device connects to the AP.  The MAC address of the connected station, as a 6-byte binary, is passed to the callback function.
* `{sta_disconnected, fun((Mac::binary()) -> term())}` A callback function which will be called when a device disconnects from the AP.  The MAC address of the disconnected station, as a 6-byte binary, is passed to the callback function.
* `{sta_ip_assigned, fun((ipv4_address()) -> term())}` A callback function which will be called when the AP assigns an IP address to a station.  The assigned IP address is passed to the callback function.

```{warning}
IPv6 addresses are not yet supported in AtomVM.
```

Callback functions are completely optional, but are highly recommended for building robust WiFi applications.  The return value from callback functions is ignored, and AtomVM provides no guarantees about the execution context (i.e., BEAM process) in which these functions are invoked.

In addition, the following optional parameters can be specified to configure the AP network:

* `{ssid_hidden, boolean()}` Whether the AP network should be not be broadcast (false, by default)
* `{max_connections, non_neg_integer()}` The maximum number of devices that can connect to this network (by default, 4)

The following example illustrates initialization of the WiFi network in AP mode.  The example program will configure the network to connect to start a WiFi network with the name `myssid` and password `mypsk`.  Events that occur during the lifecycle of the network will trigger invocations of the specified callback functions.

```erlang
Config = [
    {ap, [
        {ssid, <<"myssid">>},
        {psk,  <<"mypsk">>},
        {ap_started, fun ap_started/0},
        {sta_connected, fun sta_connected/1},
        {sta_ip_assigned, fun sta_ip_assigned/1},
        {sta_disconnected, fun sta_disconnected/1},
    ]}
],
{ok, Pid} = network:start(Config),
...
```

The following callback functions will be called when the corresponding events occur during the lifetime of the network connection.

```erlang
ap_started() ->
    io:format("AP started.~n").

sta_connected(Mac) ->
    io:format("STA connected with mac ~p~n", [Mac]).

sta_disconnected(Mac) ->
    io:format("STA disconnected with mac ~p~n", [Mac]).

sta_ip_assigned(Address) ->
    io:format("STA assigned address ~p~n", [Address]).
```

In a typical application, the network should be configured and the application should wait for the AP to report that it has started, before starting clients or services that have a dependency on the network.

### AP Mode Convenience Functions

The `network` module supports the [`network:wait_for_ap/1,2`](./apidocs/erlang/eavmlib/network.md#wait_for_ap1) convenience functions for applications that do not need robust connection management.  These functions are synchronous and will wait until the device is successfully starts an AP.  Supply the properties list specified in the `{ap, [...]}` component of the above configuration, in addition to an optional timeout (in milliseconds).

For example:

```erlang
Config = [
    {psk,  <<"mypsk">>}
],
case network:wait_for_ap(Config, 15000) of
    ok ->
        io:format("AP network started at 192.168.4.1~n");
    {error, Reason} ->
        io:format("Network initialization failed: ~p~n", [Reason])
end
```

## STA+AP mode

The `network` module can be started in both STA and AP mode.  In this case, the ESP32 device will both connect to an access point in its STA mode, and will simultaneously serve as an access point in its role in AP mode.

In order to enable both STA and AP mode, simply provide valid configuration for both modes in the configuration structure supplied to the [`network:start/1`](./apidocs/erlang/eavmlib/network.md#start1) function.

## SNTP Support

You may configure the networking layer to automatically synchronize time on the ESP32 with an NTP server accessible on the network.

To synchronize time with an NTP server, add a property list with the tag `sntp` at the top level configuration passed into the `network:start/1` function.  Specify the NTP hostname or IP address with which your device should sync using the `host` property tag.  The `host` value can be a string or binary.

You can also specify a callback function that will get called when the clock is synchronized with the SNTP server via the `synchronized` property tag.  This function takes a tuple with the updated time in seconds and microseconds.

For example:

```erlang
{sntp, [
    {host, <<"pool.ntp.org">>},
    {synchronized, fun sntp_synchronized/1}
]}
```

where the `sntp_synchronized/1` function is defined as:

```erlang
sntp_synchronized({TVSec, TVUsec}) ->
    io:format("Synchronized time with SNTP server. TVSec=~p TVUsec=~p~n", [TVSec, TVUsec]).
```

```{note}
The device must be in STA mode and connected to an access point in order to use an SNTP server on your network
or on the internet.
```

## NVS Credentials

It can become tiresome to enter an SSID and password for every application, and in general it is bad security practice to hard-wire WiFi credentials in your application source code.

You may instead store an STA or AP SSID and PSK in [non-volatile storage (NVS)](./programmers-guide.md#non-volatile-storage) on and ESP32 device.

```{caution}
Credentials are stored un-encrypted and in plaintext and should not be considered secure.  Future versions may use
encrypted NVS storage.
```

## Stopping the Network

To stop the network and free any resources in use, issue the [`stop/0`](./apidocs/erlang/eavmlib/network.md#stop0) function:

```erlang
network:stop().
```

```{caution}
Stop is currently unimplemented.
```
