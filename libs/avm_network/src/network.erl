%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(network).

-behavior(gen_server).

-export([
    wait_for_sta/0, wait_for_sta/1, wait_for_sta/2,
    wait_for_ap/0, wait_for_ap/1, wait_for_ap/2,
    sta_rssi/0,
    sta_disconnect/0,
    sta_connect/0, sta_connect/1,
    sta_status/0,
    wifi_scan/0, wifi_scan/1
]).
-export([start/1, start_link/1, stop/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).

% maximum number of channels, used to calculate the gen_server:call/3 scan timeout.
-define(DEVICE_2G_CHANNELS, 14).
-define(DEVICE_2G5G_CHANNELS, 71).

-define(DEFAULT_PASSIVE_DWELL, 360).
-define(DEFAULT_ACTIVE_DWELL, 120).
-define(GEN_RESPONSE_MS, 5000).

-type ipv4_info() :: {
    IPAddress :: inet:ip4_address(), NetMask :: inet:ip4_address(), Gateway :: inet:ip4_address()
}.
-type ip_info() :: ipv4_info().
-type bssid() :: <<_:48>>.
-type ssid() :: string() | binary().
%% SSIDs can be strings or binary strings up to 32 characters long.

-type ssid_config() :: {ssid, ssid()}.
-type psk_config() :: {psk, string() | binary()}.
-type app_managed_config() :: managed | {managed, boolean()}.
%% Setting `{managed, true}' or including the atom `managed' in the `sta_config()' will signal to
%% the driver that sta mode connections are managed in the user application, allowing to start the
%% driver in STA (or AP+STA) mode, configuring `ssid' and `psk' if provided, but not immediately
%% connecting to the access point. If `ssid` and `psk` are provided in `managed` mode, this
%% configuration will be used when starting a connection using `sta_connect/0`. When using this
%% mode of operation applications will likely need to provide an `sta_disconnected_config()' to
%% replace the default callback (which attempts to reconnect to the last network) and instead scan
%% for available networks, or use some other means of determining when, and which network to
%% connect to.

-type dhcp_hostname_config() :: {dhcp_hostname, string() | binary()}.
-type sta_connected_config() :: {connected, fun(() -> term())}.
-type sta_beacon_timeout_config() :: {beacon_timeout, fun(() -> term())}.
-type sta_disconnected_config() :: {disconnected, fun(() -> term())}.
%% If no callback is configured the default behavior when the connection to an access point is
%% lost is to attempt to reconnect. If a callback is provided these automatic re-connections will
%% no longer occur, and the application must use `network:sta_connect/0' to reconnect to the last
%% access point, or use `network:sta_connect/1' to connect to a new access point in a manner
%% determined by the application.
-type scan_done_config() ::
    {scan_done, fun((scan_results() | {error, Reason :: term()}) -> term()) | pid()}.
%% If no callback is configured for scan done events then all scans will block the caller until
%% the scan is complete. If a `scan_done' event callback is configured either at startup, or through
%% `sta_connect/1', then calls to `wifi_scan/0,1' will return `ok' after a scan is initiated, and
%% the callback will receive scan results or error tuples. If a `pid()' is used in the callback
%% configuration `{scan_results, Results :: scan_results()}' or
%% `{scan_results, {error, Reason :: term()}}' will be sent to the receiver.

-type sta_got_ip_config() :: {got_ip, fun((ip_info()) -> term())}.
-type sta_scan_config() ::
    {default_scan_results, 1..64}
    %% The maximum allowed number of scan results varies by chip-set. See: `wifi_scan/1'.
    | {scan_dwell_ms, 1..1500}
    | scan_show_hidden
    | {scan_show_hidden, boolean()}
    | scan_passive
    | {scan_passive, boolean()}.
-type sta_config_property() ::
    app_managed_config()
    | ssid_config()
    | psk_config()
    | dhcp_hostname_config()
    | sta_scan_config()
    | sta_connected_config()
    | sta_beacon_timeout_config()
    | sta_disconnected_config()
    | sta_got_ip_config()
    | scan_done_config().
-type sta_config() :: {sta, [sta_config_property()]}.

-type mac() :: binary().
-type ghz24_channel() :: 1..14.
% This is the global 2.4 Ghz WiFI channel range, check your local jurisdiction for allowed channels in your geographic region.

-type ghz5_20mhz_channel() ::
    32
    | 36
    | 40
    | 44
    | 48
    | 52
    | 56
    | 60
    | 64
    | 68
    | 96
    | 104
    | 108
    | 112
    | 116
    | 120
    | 122
    | 128
    | 132
    | 136
    | 140
    | 144
    | 149
    | 153
    | 157
    | 161
    | 165
    | 169
    | 173
    | 177.
% This is the global 5 Ghz WiFI channel range when using 20Mhz bandwidth channels, check your local jurisdiction for allowed channels in your geographic region.

-type ghz5_40mhz_channel() ::
    38 | 46 | 54 | 62 | 102 | 110 | 118 | 126 | 134 | 142 | 151 | 159 | 167 | 175.
% This is the global 5 Ghz WiFI channel range when using 40Mhz bandwidth channels, check your local jurisdiction for allowed channels in your geographic region.

-type ghz5_80mhz_channel() :: 42 | 58 | 74 | 90 | 106 | 122 | 138 | 155 | 171.
% This is the global 5 Ghz WiFI channel range when using 80Mhz bandwidth channels, check your local jurisdiction for allowed channels in your geographic region.

-type ghz5_160mhz_channel() :: 50 | 82 | 114 | 163.
% This is the global 5 Ghz WiFI channel range when using 160Mhz bandwidth channels, check your local jurisdiction for allowed channels in your geographic region.

-type wifi_channel() ::
    ghz24_channel()
    | ghz5_20mhz_channel()
    | ghz5_40mhz_channel()
    | ghz5_80mhz_channel()
    | ghz5_160mhz_channel().

-type ap_channel_cfg() :: {ap_channel, wifi_channel()}.
-type ap_ssid_hidden_config() :: {ap_ssid_hidden, boolean()}.
-type ap_max_connections_config() :: {ap_max_connections, non_neg_integer()}.
-type ap_started_config() :: {ap_started, fun(() -> term())}.
-type ap_sta_connected_config() :: {sta_connected, fun((mac()) -> term())}.
-type ap_sta_disconnected_config() :: {sta_disconnected, fun((mac()) -> term())}.
-type ap_sta_ip_assigned_config() :: {sta_ip_assigned, fun((inet:ip4_address()) -> term())}.
-type ap_config_property() ::
    ssid_config()
    | psk_config()
    | ap_channel_cfg()
    | ap_ssid_hidden_config()
    | ap_max_connections_config()
    | ap_started_config()
    | ap_sta_connected_config()
    | ap_sta_disconnected_config()
    | ap_sta_ip_assigned_config().
-type ap_config() :: {ap, [ap_config_property()]}.

-type sntp_host_config() :: {host, string() | binary()}.
-type sntp_synchronized_config() ::
    {synchronized, fun(({non_neg_integer(), non_neg_integer()}) -> term())}.
-type sntp_config_property() ::
    sntp_host_config()
    | sntp_synchronized_config().
-type sntp_config() :: {sntp, [sntp_config_property()]}.

-type mdns_hostname_config() :: {host, string() | binary()}.
-type mdns_ttl_config() :: {ttl, pos_integer()}.
-type mdns_config_property() ::
    mdns_hostname_config()
    | mdns_ttl_config().
-type mdns_config() :: {mdns, [mdns_config_property()]}.

-type network_config() :: [sta_config() | ap_config() | sntp_config() | mdns_config()].

-type dbm() :: integer().
%% `dbm()' decibel-milliwatts (or dBm) will typically be a negative number, but in the presence of
%% a powerful signal this can be a positive number. A level of 0 dBm corresponds to the power of 1
%% milliwatt. A 10 dBm decrease in level is equivalent to a ten-fold decrease in signal power.
-type sta_status() ::
    associated | connected | connecting | degraded | disconnected | disconnecting | inactive.

-type scan_option() ::
    {results, 1..64}
    | {dwell, 1..1500}
    | show_hidden
    | {show_hidden, boolean()}
    | passive
    | {passive, boolean()}.
%% The `results' key is used to set the maximum number of networks returned in the networks list,
%% and the `dwell' is used to set the dwell time (in milliseconds) spent on each channel. The
%% maximum number of results varies by chip, see: `network:wifi_scan/1'. The option `show_hidden'
%% will also include hidden networks in the scan results. Default options are:
%% `[{results, 6}, {dwell, 120}, {passive, false}, {show_hidden, false}]', if `passive' (or
%% `{passive, true}') is used the default dwell time per channel is 360 ms.
-type scan_options() :: [scan_option()].

-type auth_type() ::
    open
    | wep
    | wpa_psk
    | wpa2_psk
    | wpa_wpa2_psk
    | eap
    | wpa3_psk
    | wpa2_wpa3_psk
    | wapi
    | owe
    | wpa3_enterprise_192
    | wpa3_ext_psk
    | wpa3_ext_psk_mixed
    | dpp
    | wpa_enterprise
    | wpa3_enterprise
    | wpa2_wpa3_enterprise.

-type network_properties() ::
    #{
        authmode := auth_type(),
        bssid := bssid(),
        channel := wifi_channel(),
        hidden := boolean(),
        rssi := dbm(),
        ssid := ssid()
    }.
%% A map of network properties with the keys: `ssid', `rssi', `authmode', `bssid', `channel' and `hidden'
-type scan_results() :: {NetworksDiscovered :: 0..64, [network_properties()]}.

-record(state, {
    config :: network_config(),
    port :: port(),
    ref :: reference(),
    sta_ip_info :: ip_info() | undefined,
    mdns :: pid() | undefined,
    sta_state :: sta_status(),
    scan_receiver ::
        {callback, pid() | fun((scan_results() | {error, term()}) -> term())}
        | {reply, gen_server:from()}
        | undefined
        | blocked
}).

%%-----------------------------------------------------------------------------
%% @doc     Equivalent to wait_for_sta(15000).
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_sta() -> {ok, ip_info()} | {error, Reason :: term()}.
wait_for_sta() ->
    wait_for_sta(15000).

%%-----------------------------------------------------------------------------
%% @param   TimeoutOrStaConfig The STA network configuration or timeout in ms.
%% @doc     Equivalent to wait_for_sta([], Timeout) or wait_for_sta(StaConfig, 15000).
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_sta(TimeoutOrStaConfig :: non_neg_integer() | [sta_config_property()]) ->
    {ok, ip_info()} | {error, Reason :: term()}.
wait_for_sta(Timeout) when is_integer(Timeout) ->
    wait_for_sta([], Timeout);
wait_for_sta(StaConfig) when is_list(StaConfig) ->
    wait_for_sta(StaConfig, 15000).

%%-----------------------------------------------------------------------------
%% @param   StaConfig The STA network configuration
%% @param   Timeout amount of time in milliseconds to wait for a connection
%% @returns {ok, IpInfo}, if the network interface was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Start a network interface in station mode and wait for a connection to be established
%%
%%          This function will start a network interface in station mode, and will wait
%%          for a connection to be established.  This is a convenience function,
%%          for applications that do not need to be notified of connectivity
%%          changes in the network.
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_sta(StaConfig :: [sta_config_property()], Timeout :: non_neg_integer()) ->
    {ok, ip_info()} | {error, Reason :: term()}.
wait_for_sta(StaConfig, Timeout) ->
    Self = self(),
    NewStaConfig = [
        {connected, fun() -> Self ! connected end},
        {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
        {disconnected, fun() -> Self ! disconnected end}
        | StaConfig
    ],
    Config = [{sta, NewStaConfig}],
    case start(Config) of
        {ok, _Pid} ->
            wait_for_ip(Timeout);
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @doc     Equivalent to wait_for_ap(15000).
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_ap() -> ok | {error, Reason :: term()}.
wait_for_ap() ->
    wait_for_ap(15000).

%%-----------------------------------------------------------------------------
%% @param   TimeoutOrApConfig The AP network configuration or timeout in ms.
%% @doc     Equivalent to wait_for_ap([], Timeout) or wait_for_ap(StaConfig, 15000).
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_ap(TimeoutOrApConfig :: non_neg_integer() | [ap_config_property()]) ->
    ok | {error, Reason :: term()}.
wait_for_ap(Timeout) when is_integer(Timeout) ->
    wait_for_ap([], Timeout);
wait_for_ap(ApConfig) when is_list(ApConfig) ->
    wait_for_ap(ApConfig, 15000).

%%-----------------------------------------------------------------------------
%% @param   ApConfig The AP network configuration
%% @param   Timeout amount of time in milliseconds to wait for a connection
%% @returns ok, when the network has started the AP, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Start a network interface in access point mode and wait the AP to be
%%          up and running
%%
%%          This function will start a network interface in AP mode, and will wait
%%          until the network is up and ready to be connected.  This is a convenience function,
%%          for applications that do not need to be notified of connectivity
%%          changes in the network.
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_ap(ApConfig :: [ap_config_property()], Timeout :: non_neg_integer()) ->
    ok | {error, Reason :: term()}.
wait_for_ap(ApConfig, Timeout) ->
    Self = self(),
    NewApConfig = [
        {ap_started, fun() -> Self ! ap_started end}
        | ApConfig
    ],
    Config = [{ap, NewApConfig}],
    case start(Config) of
        {ok, _Pid} ->
            wait_for_ap_started(Timeout);
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Config The network configuration
%% @returns ok, if the network interface was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Start a network interface.
%%
%%          This function will start a network interface, which will attempt to
%%          connect to an AP endpoint in the background. If the `managed'
%%          option us used the driver will be started, but the connection will
%%          be delayed until `network:sta_connect/0,1' is used to start the
%%          connection. Specify callback functions to receive definitive
%%          information that the connection succeeded; specify a
%%          `sta_disconnected_config()' in the `sta_config()' to manage
%%          re-connections in the application, rather than the default
%%          automatic attempt to reconnect until a connection is reestablished.
%%
%%          See the AtomVM Network Programming Manual for more information.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config :: network_config()) -> {ok, pid()} | {error, Reason :: term()}.
start(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

-spec start_link(Config :: network_config()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%-----------------------------------------------------------------------------
%% @returns `ok', if the network disconnects from the access point, or
%%          `{error, Reason}' if a failure occurred.
%% @doc     Disconnect from access point.
%%
%%          This will terminate a connection to an access point.
%%
%%          Note: Using this function without providing an `sta_disconnected_config()'
%%          in the `sta_config()' will result in the driver immediately attempting to
%%          reconnect to the same access point again.
%% @end
%%-----------------------------------------------------------------------------
-spec sta_disconnect() -> ok | {error, Reason :: term()}.
sta_disconnect() ->
    gen_server:call(?SERVER, halt_sta, 65000).

%%-----------------------------------------------------------------------------
%% @param   Config The new station mode network configuration
%% @returns ok, if the network interface was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Connect to a new access point after the network driver has been started.
%%
%%          This function will attempt to connect to a new AP endpoint in the
%%          background.
%% @end
%%-----------------------------------------------------------------------------
-spec sta_connect(Config :: network_config() | [sta_config_property()]) ->
    ok | {error, Reason :: term()}.
sta_connect(Config) ->
    gen_server:call(?SERVER, {connect, Config}, 65000).

%%-----------------------------------------------------------------------------
%% @returns ok, if the network interface was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Connect to an access point after a network disconnection.
%%
%%          This function will attempt to connect, in the background, to the
%%          last AP endpoint that was configured.
%% @end
%%-----------------------------------------------------------------------------
-spec sta_connect() -> ok | {error, Reason :: term()}.
sta_connect() ->
    gen_server:call(?SERVER, connect).

%%-----------------------------------------------------------------------------
%% @returns ok, if the network interface was stopped, or {error, Reason} if
%%          a failure occurred.
%% @doc     Stop a network interface.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    case erlang:whereis(network) of
        Pid when is_pid(Pid) ->
            Monitor = monitor(process, Pid),
            try gen_server:call(Pid, stop_network) of
                ok ->
                    receive
                        {'DOWN', Monitor, process, Pid, normal} ->
                            ok;
                        {'DOWN', Monitor, process, Pid, Reason} ->
                            {error, Reason}
                    after 5000 ->
                        erlang:demonitor(Monitor, [flush]),
                        {error, timeout}
                    end;
                Error ->
                    erlang:demonitor(Monitor, [flush]),
                    Error
            catch
                exit:{noproc, _} ->
                    erlang:demonitor(Monitor, [flush]),
                    ok;
                exit:{timeout, _} ->
                    erlang:demonitor(Monitor, [flush]),
                    {error, timeout};
                exit:Reason ->
                    erlang:demonitor(Monitor, [flush]),
                    {error, {exit, Reason}}
            end;
        _ ->
            ok
    end.

%%-----------------------------------------------------------------------------
%% @returns {ok, Rssi} in dBm, or {error, Reason}.
%%
%% @doc     Get the rssi information of AP to which the device is associated with.
%% @end
%%-----------------------------------------------------------------------------
-spec sta_rssi() -> {ok, Rssi :: dbm()} | {error, Reason :: term()}.
sta_rssi() ->
    case whereis(network_port) of
        undefined ->
            {error, network_down};
        Port ->
            Ref = make_ref(),
            Port ! {self(), Ref, rssi},
            receive
                {Ref, {error, Reason}} -> {error, Reason};
                {Ref, {rssi, Rssi}} -> {ok, Rssi};
                Other -> {error, Other}
            end
    end.

%%-----------------------------------------------------------------------------
%% @returns ConnectionState :: sta_status().
%%
%% @doc Get the connection status of the sta interface.
%%
%% Results will be one of: `associated', `connected', `connecting', `degraded',
%% `disconnected', `disconnecting', or `inactive'. The state `associated' indicates
%% that the station is connected to an access point, but does not yet have an IP address.
%% A status of `degraded' indicates that the connection has experienced at least one
%% beacon timeout event during the current connection session. This does not necessarily
%% mean the connection is still in a poor state, but it might be helpful diagnosing
%% problems with networked applications.
%% @end
%%-----------------------------------------------------------------------------
-spec sta_status() -> Status :: sta_status().
sta_status() ->
    gen_server:call(?SERVER, sta_status).

%%-----------------------------------------------------------------------------
%% @param   Options is a `scan_options()' list
%% @returns `ok', `{ok, Result}' tuple, or `{error, Reason}' if a failure occurred.
%%
%% @doc     Scan for available WiFi networks.
%%
%% The network must first be started in sta or sta+ap mode before scanning for access points. While
%% a scan is in progress network traffic will be inhibited for clients connected to the esp32
%% access point (in using ap+sta mode), but should not cause an active client connection to be
%% lost. Espressif's documentation recommends not exceeding 1500 ms per-channel scan times or
%% network connections may be lost, this is enforced as a hard limit. The return when no callback
%% is used is a tuple `{ok, Results}', where Results is a tuple with the number of discovered
%% networks and a list of networks, which may be shorter than the size of the discovered networks
%% if a smaller `MaxAPs' was used. If a callback function is used it will receive the bare results
%% tuple, (no `ok') or an error tuple. If a `pid()' is used to receive callback results, the
%% results will be wrapped in a tuple with `scan_results', for example:
%% `{scan_results, {NumberResults, Networks}}'. The network maps in the list consist of network
%% name and other network properties (direct call without a configured callback):
%%
%% `{ok, {
%%      NumberResults,
%%      [#{authmode := Mode, bssid := BSSID, channel := ChNum, hidden := Bool, rssi := DBm, ssid := SSID}, ...]
%% }}'
%%
%% In the event of a failure `{error, Reason :: term()}' will be returned. If the network is
%% stopped while a scan is in progress, the caller or callback may receive either a successful scan
%% result, or `{error, scan_canceled}'. In the unlikely circumstance that internal de-registration of
%% the scan done event handler fails, all future scans will be denied with `{error, blocked}'.
%% Only one scan may be active at a time. Starting a second scan before results from the first are
%% returned will result in `{error, busy}'.
%%
%% To minimize the risk of out-of-memory errors, this driver limits the maximum number of returned
%% networks depending on the target and memory configuration:
%%
%% <table>
%%   <tr> <th>Chip</th> <th>Maximum number of networks</th> </tr>
%%   <tr> <td>`ANY with PSRAM allocatable'</td> <td>`64'</td> </tr>
%%   <tr> <td>`ESP32'</td> <td>`20'</td> </tr>
%%   <tr> <td>`ESP32-C2'</td> <td>`10'</td> </tr>
%%   <tr> <td>`ESP32-C3'</td> <td>`20'</td> </tr>
%%   <tr> <td>`ESP32-C5'</td> <td>`14'</td> </tr>
%%   <tr> <td>`ESP32-C6'</td> <td>`20'</td> </tr>
%%   <tr> <td>`ESP32-C61'</td> <td>`14'</td> </tr>
%%   <tr> <td>`ESP32-P4'</td> <td>`64'</td> </tr>
%%   <tr> <td>`ESP32-S2'</td> <td>`14'</td> </tr>
%%   <tr> <td>`ESP32-S3'</td> <td>`20'</td> </tr>
%% </table>
%%
%% Optionally a callback may be configured for scan done events. In this case this function will
%% return `ok' immediately after a scan is successfully initiated, or an error if the scan was
%% not successfully started, avoiding having the caller remain blocked while waiting for results.
%% The callback function will receive the bare results tuple without being wrapped in an `ok'
%% tuple. In the event of an error while scanning the callback will receive an `{error, Reason}'
%% tuple. If a `pid()' is used for callback the result will be wrapped in a `scan_results' tuple.
%% For example: `{scan_results, {1, [#{...}]}}'
%%
%% For convenience `network:wifi_scan/0' may be used to scan with default options.  The driver must
%% already be started, and not in the process of connecting to an access when this function is
%% used.
%%
%% Note, if a long dwell time is used, the return time for this function can be considerably longer
%% than the default gen_server timeout, especially when performing a passive scan. Short dwell
%% times can easily miss networks, so applications need to be adjusted for their environment.
%% Passive scans always use the full dwell time for each channel, active scans with a dwell time of
%% more than 240 milliseconds will have a minimum dwell of 1/2 the maximum dwell time set by the
%% `dwell' option. All scans will have a 15 second minimum timeout (25 seconds for 5Ghz capable
%% devices). The timeout will be the greater value of the minimum timeout or the value obtained from
%% the following formula:
%%
%% <pre>
%%     Timeout = (dwell * NumChannels) + 5000
%% </pre>
%%
%% 2.4Ghz wifi chips support a total of 14 channels, while dual-band (2.4Ghz + 5Ghz) capable chips
%% support up to 71 channels, in global ("world safe") mode. Be advised that network scans on 5Ghz
%% capable devices can take considerably longer than 2.4Ghz only devices, even when using short
%% dwell times. The use of a `scan_done' event callback is strongly encouraged.
%%
%% The actual number of channels scanned will be determined by the country code, which currently
%% does not have a configurable option, but is set to `01' (world safe mode) and will automatically
%% change to match the devices connection to an access point.
%%
%% The default scan options may be configured by adding `sta_scan_config()' options to the
%% `sta_config()'.
%%
%% Warning: This feature is not yet supported on platforms other than ESP32, and will return
%% `{error, unsupported_platform}'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_scan(Options :: scan_options()) ->
    ok
    | {ok, scan_results()}
    | {error, Reason :: term()}.
wifi_scan(Options) ->
    Platform =
        case atomvm:platform() of
            esp32 -> ok;
            Platform0 -> Platform0
        end,
    Passive = proplists:get_bool(passive, Options),
    Dwell =
        case {proplists:get_value(dwell, Options), Passive} of
            {undefined, false} -> ?DEFAULT_ACTIVE_DWELL;
            {undefined, _} -> ?DEFAULT_PASSIVE_DWELL;
            {Value, _} -> Value
        end,
    case {(is_integer(Dwell) andalso Dwell >= 1 andalso Dwell =< 1500), Platform} of
        {false, _} ->
            {error, badarg};
        {true, ok} ->
            {NumChannels, DefaultTimeout} = get_num_channels_timeout(),
            ComputedTimeout = (Dwell * NumChannels),
            Timeout = erlang:max(DefaultTimeout, ComputedTimeout) + ?GEN_RESPONSE_MS,
            case erlang:whereis(?SERVER) of
                undefined -> {error, network_not_started};
                Pid -> gen_server:call(Pid, {scan, Options}, Timeout)
            end;
        {true, _} ->
            {error, unsupported_platform}
    end.

%% @doc Equivalent to `wifi_scan/1' with `sta_scan_config()' options set in `sta_config()'.
%% @end
-spec wifi_scan() ->
    ok
    | {ok, scan_results()}
    | {error, Reason :: term()}.
wifi_scan() ->
    case atomvm:platform() of
        esp32 ->
            Config =
                try
                    NetConfig = gen_server:call(?SERVER, get_config),
                    proplists:get_value(sta, NetConfig)
                catch
                    exit:{noproc, _} -> {error, network_not_started}
                end,
            case Config of
                {error, _} = Error ->
                    Error;
                undefined ->
                    {error, no_sta_mode};
                StaCfg ->
                    Results = proplists:get_value(default_scan_results, StaCfg, 6),
                    Passive = proplists:get_bool(scan_passive, StaCfg),
                    DefaultDwell =
                        case Passive of
                            false -> ?DEFAULT_ACTIVE_DWELL;
                            _ -> ?DEFAULT_PASSIVE_DWELL
                        end,
                    Dwell = proplists:get_value(scan_dwell_ms, StaCfg, DefaultDwell),
                    Hidden = proplists:get_bool(scan_show_hidden, StaCfg),
                    wifi_scan([
                        {results, Results},
                        {dwell, Dwell},
                        {show_hidden, Hidden},
                        {passive, Passive}
                    ])
            end;
        _ ->
            {error, unsupported_platform}
    end.

%%
%% gen_server callbacks
%%

%% @hidden
init(Config) ->
    Port = get_port(),
    Ref = make_ref(),
    Status =
        case proplists:get_value(sta, Config) of
            undefined ->
                inactive;
            STA ->
                case proplists:get_value(managed, STA, false) of
                    false ->
                        connecting;
                    true ->
                        disconnected
                end
        end,
    {ok, #state{config = Config, port = Port, ref = Ref, sta_state = Status},
        {continue, start_port}}.

%% @hidden
handle_continue(start_port, #state{config = Config, port = Port, ref = Ref} = State) ->
    Port ! {self(), Ref, {start, Config}},
    receive
        {Ref, ok} ->
            {noreply, State};
        {Ref, {error, Reason}} ->
            {stop, {start_port_failed, Reason}, State}
    end.

%% @hidden
handle_call(halt_sta, _From, #state{ref = Ref} = State) ->
    network_port ! {self(), Ref, halt_sta},
    wait_halt_sta_reply(Ref, State#state{sta_state = disconnecting});
handle_call(connect, _From, #state{config = Config, ref = Ref} = State) ->
    network_port ! {self(), Ref, {connect, Config}},
    wait_connect_reply(Ref, Config, State#state{sta_state = connecting});
handle_call({connect, Config}, _From, #state{config = OldConfig, ref = Ref} = State) ->
    case update_config(OldConfig, Config) of
        {error, Reason} ->
            {reply, {error, Reason}, State};
        NewConfig ->
            network_port ! {self(), Ref, {connect, NewConfig}},
            wait_connect_reply(Ref, NewConfig, State#state{
                sta_state = connecting, config = NewConfig
            })
    end;
handle_call(sta_status, _From, State) ->
    {reply, State#state.sta_state, State};
handle_call(
    {scan, ScanOpts}, From, #state{ref = Ref, scan_receiver = undefined, config = Config} = State
) ->
    case proplists:get_value(sta, Config) of
        undefined ->
            {reply, {error, no_sta_mode}, State};
        StaConfig ->
            case proplists:get_value(scan_done, StaConfig) of
                undefined ->
                    network_port ! {self(), Ref, {scan, ScanOpts}},
                    {noreply, State#state{scan_receiver = {reply, From}}};
                FunOrPid ->
                    network_port ! {self(), Ref, {scan, [{reply, true} | ScanOpts]}},
                    wait_scan_start_reply(Ref, {callback, FunOrPid}, State)
            end
    end;
handle_call({scan, _ScanOpts}, _From, #state{scan_receiver = blocked} = State) ->
    {reply, {error, blocked}, State};
handle_call({scan, _ScanOpts}, _From, State) ->
    {reply, {error, busy}, State};
handle_call(get_config, _From, #state{config = Config} = State) ->
    {reply, Config, State};
handle_call(get_scan_state, _From, #state{scan_receiver = Active} = State) ->
    Scanning =
        case Active of
            undefined -> inactive;
            blocked -> blocked;
            _ -> active
        end,
    {reply, Scanning, State};
handle_call(stop_network, From, #state{scan_receiver = Receiver} = State) when
    Receiver =:= undefined; Receiver =:= blocked
->
    gen_server:reply(From, ok),
    {stop, normal, State};
handle_call(stop_network, From, #state{ref = Ref} = State) ->
    network_port ! {self(), Ref, {cancel_scan, {shutdown, From}}},
    {noreply, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({Ref, sta_connected} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_connected_callback(Config),
    {noreply, State#state{sta_state = associated}};
handle_info({Ref, sta_beacon_timeout} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_beacon_timeout_callback(Config),
    {noreply, State#state{sta_state = degraded}};
handle_info({Ref, sta_disconnected} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_disconnected_callback(Config),
    {noreply, State#state{sta_state = disconnected, sta_ip_info = undefined}};
handle_info({Ref, {sta_got_ip, IpInfo}} = _Msg, #state{ref = Ref, config = Config} = State0) ->
    maybe_sta_got_ip_callback(Config, IpInfo),
    State1 = State0#state{sta_ip_info = IpInfo, sta_state = connected},
    State2 = maybe_start_mdns(State1),
    {noreply, State2};
handle_info({Ref, ap_started} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_ap_started_callback(Config),
    {noreply, State};
handle_info({Ref, {ap_sta_connected, Mac}} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_ap_sta_connected_callback(Config, Mac),
    {noreply, State};
handle_info(
    {Ref, {ap_sta_disconnected, Mac}} = _Msg, #state{ref = Ref, config = Config} = State
) ->
    maybe_ap_sta_disconnected_callback(Config, Mac),
    {noreply, State};
handle_info(
    {Ref, {ap_sta_ip_assigned, Address}} = _Msg, #state{ref = Ref, config = Config} = State
) ->
    maybe_ap_sta_ip_assigned_callback(Config, Address),
    {noreply, State};
handle_info({Ref, {sntp_sync, TimeVal}} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sntp_sync_callback(Config, TimeVal),
    {noreply, State};
handle_info(
    {Ref, {scan_results, {error, {unregister_handler, _}}} = Msg}, #state{ref = Ref} = State
) ->
    scan_reply_or_callback(Msg, State),
    {noreply, State#state{scan_receiver = blocked}};
handle_info(
    {Ref, {scan_results, _Results} = Msg}, #state{ref = Ref, scan_receiver = {reply, _}} = State
) ->
    scan_reply_or_callback(Msg, State),
    {noreply, State#state{scan_receiver = undefined}};
handle_info({Ref, {scan_results, _Results} = Msg}, #state{ref = Ref} = State) ->
    spawn(fun() -> scan_reply_or_callback(Msg, State) end),
    {noreply, State#state{scan_receiver = undefined}};
handle_info(
    {Ref, {scan_canceled, {Next, ReplyTo}, ok}}, #state{ref = Ref, scan_receiver = Receiver} = State
) ->
    ScanMsg = {scan_results, {error, scan_canceled}},
    case Receiver of
        {reply, _} ->
            scan_reply_or_callback(ScanMsg, State);
        _ ->
            spawn(fun() -> scan_reply_or_callback(ScanMsg, State) end)
    end,
    gen_server:reply(ReplyTo, ok),
    case Next of
        shutdown ->
            {stop, normal, State#state{scan_receiver = undefined}};
        _ ->
            {noreply, State#state{scan_receiver = undefined}}
    end;
handle_info(
    {Ref, {scan_canceled, {shutdown, ReplyTo}, {error, _Reason}}}, #state{ref = Ref} = State
) ->
    gen_server:reply(ReplyTo, {error, cancel_scan_failed}),
    {noreply, State};
handle_info({Ref, {scan_canceled, {_, ReplyTo}, Error}}, #state{ref = Ref} = State) ->
    gen_server:reply(ReplyTo, Error),
    {noreply, State};
%% catch oom errors when wifi_scan cannot allocate `scan_results` atom
handle_info({Ref, {error, _} = Error}, #state{ref = Ref, scan_receiver = {reply, _}} = State) ->
    scan_reply_or_callback({scan_results, Error}, State),
    {noreply, State#state{scan_receiver = undefined}};
handle_info({Ref, {error, _} = Error}, #state{ref = Ref} = State) ->
    spawn(fun() -> scan_reply_or_callback({scan_results, Error}, State) end),
    {noreply, State#state{scan_receiver = undefined}};
handle_info(Msg, State) ->
    io:format("Received spurious message ~p~n", [Msg]),
    {noreply, State}.

%% @hidden
%% Wait for port to be closed
terminate(_Reason, State) ->
    Ref = make_ref(),
    Port = State#state.port,
    PortMonitor = erlang:monitor(port, Port),
    network_port ! {?SERVER, Ref, stop},
    wait_for_port_close(PortMonitor, Port).

%% @private
wait_connect_reply(Ref, NewConfig, State) ->
    receive
        {Ref, ok} ->
            {reply, ok, State#state{config = NewConfig}};
        {Ref, {error, _Reason} = ER} ->
            {reply, ER, State#state{sta_state = disconnected}}
    after 60000 ->
        {reply, {error, timeout}, State#state{sta_state = disconnected}}
    end.

%% @private
wait_halt_sta_reply(Ref, State) ->
    receive
        {Ref, ok} ->
            {reply, ok, State#state{sta_state = disconnected}};
        {Ref, {error, _Reason} = Error} ->
            {reply, Error, State}
    after 60000 ->
        {reply, {error, timeout}, State}
    end.

wait_for_port_close(PortMonitor, Port) ->
    receive
        {'DOWN', PortMonitor, port, Port, _DownReason} ->
            ok;
        _Other ->
            % Handle unexpected messages if necessary
            wait_for_port_close(PortMonitor, Port)
        % Timeout after 1 second just in case.
    after 1000 ->
        {error, timeout}
    end.

wait_scan_start_reply(Ref, Dispatch, State) ->
    receive
        {Ref, ok} ->
            {reply, ok, State#state{scan_receiver = Dispatch}};
        {Ref, {error, _} = Error} ->
            {reply, Error, State#state{scan_receiver = undefined}};
        {Ref, {scan_results, {error, _} = Error}} ->
            {reply, Error, State#state{scan_receiver = undefined}}
    after 10000 ->
        {reply, {error, timeout}, State#state{scan_receiver = undefined}}
    end.

%%
%% Internal operations
%%

scan_reply_or_callback({scan_results, Results} = Msg, #state{scan_receiver = Dispatch} = _State) ->
    case Dispatch of
        {reply, {Pid, _} = From} when is_pid(Pid) ->
            case Results of
                {error, _} ->
                    gen_server:reply(From, Results);
                _ ->
                    gen_server:reply(From, {ok, Results})
            end;
        {callback, Pid} when is_pid(Pid) ->
            Pid ! Msg;
        {callback, Fun} when is_function(Fun, 1) ->
            Fun(Results);
        _ ->
            ok
    end,
    ok.

%% @private
maybe_sta_connected_callback(Config) ->
    maybe_callback0(connected, proplists:get_value(sta, Config)).

%% @private
maybe_sta_beacon_timeout_callback(Config) ->
    maybe_callback0(beacon_timeout, proplists:get_value(sta, Config)).

%% @private
maybe_sta_disconnected_callback(Config) ->
    maybe_callback2(
        disconnected, proplists:get_value(sta, Config), fun sta_disconnected_default_callback/0
    ).

%% @private
maybe_sta_got_ip_callback(Config, IpInfo) ->
    maybe_callback1({got_ip, IpInfo}, proplists:get_value(sta, Config)).

%% @private
maybe_ap_started_callback(Config) ->
    maybe_callback0(ap_started, proplists:get_value(ap, Config)).

%% @private
maybe_ap_sta_connected_callback(Config, Mac) ->
    maybe_callback1({sta_connected, Mac}, proplists:get_value(ap, Config)).

%% @private
maybe_ap_sta_disconnected_callback(Config, Mac) ->
    maybe_callback1({sta_disconnected, Mac}, proplists:get_value(ap, Config)).

%% @private
maybe_ap_sta_ip_assigned_callback(Config, Address) ->
    maybe_callback1({sta_ip_assigned, Address}, proplists:get_value(ap, Config)).

%% @private
maybe_sntp_sync_callback(Config, TimeVal) ->
    maybe_callback1({synchronized, TimeVal}, proplists:get_value(sntp, Config)).

%% @private
maybe_start_mdns(#state{mdns = MDNSResponder} = State) when is_pid(MDNSResponder) ->
    mdns:stop(MDNSResponder),
    maybe_start_mdns(State#state{mdns = undefined});
maybe_start_mdns(#state{config = Config, sta_ip_info = {InterfaceAddr, _, _}} = State) ->
    case proplists:get_value(mdns, Config) of
        undefined ->
            State;
        MDNSConfig ->
            MDNSMap0 = #{
                hostname => proplists:get_value(hostname, MDNSConfig), interface => InterfaceAddr
            },
            MDNSMap1 =
                case proplists:get_value(ttl, MDNSConfig) of
                    undefined -> MDNSMap0;
                    TTL -> MDNSMap0#{ttl => TTL}
                end,
            case mdns:start_link(MDNSMap1) of
                {ok, Pid} ->
                    State#state{mdns = Pid};
                {error, _} ->
                    State
            end
    end.

%% @private
maybe_callback0(_Key, undefined) ->
    ok;
maybe_callback0(Key, Config) ->
    case proplists:get_value(Key, Config) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            Pid ! Key;
        Fun when is_function(Fun) ->
            spawn(fun() -> Fun() end)
    end.

%% @private
maybe_callback1(_KeyArg, undefined) ->
    ok;
maybe_callback1({Key, Arg} = Msg, Config) ->
    case proplists:get_value(Key, Config) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            Pid ! Msg;
        Fun when is_function(Fun) ->
            spawn(fun() -> Fun(Arg) end)
    end.

%% @private
maybe_callback2(_Key, undefined, _Default) ->
    ok;
maybe_callback2(Key, Config, Default) ->
    case proplists:get_value(Key, Config) of
        undefined when is_function(Default) ->
            spawn(fun() -> Default() end);
        Pid when is_pid(Pid) ->
            Pid ! Key;
        Fun when is_function(Fun) ->
            spawn(fun() -> Fun() end);
        _ ->
            ok
    end.

%% @private
-spec get_port() -> port().
get_port() ->
    case whereis(network_port) of
        undefined ->
            open_port();
        Port ->
            Port
    end.

%% @private
-spec open_port() -> port().
open_port() ->
    Port = erlang:open_port({spawn, "network"}, []),
    %Pid = spawn(?MODULE, simulation_loop, []),
    erlang:register(network_port, Port),
    Port.

%% @private
-spec update_config(
    OldConfig :: network_config() | [sta_config_property()], NewConfig :: network_config()
) -> UpdatedConfig :: network_config() | {error, Reason :: term()}.
update_config(OldConfig, NewConfig) ->
    try
        OldSTA = proplists:get_value(sta, OldConfig, []),
        NewSTA =
            case proplists:get_value(sta, NewConfig) of
                undefined ->
                    SSID =
                        case proplists:get_value(ssid, NewConfig) of
                            undefined ->
                                case proplists:get_value(ssid, OldSTA) of
                                    undefined ->
                                        error(no_ssid);
                                    OldSsid ->
                                        OldSsid
                                end;
                            Ssid ->
                                Ssid
                        end,
                    case proplists:get_value(psk, NewConfig) of
                        undefined ->
                            update_opts(OldSTA, [{ssid, SSID}]);
                        PSK ->
                            update_opts(OldSTA, [{ssid, SSID}, {psk, PSK}])
                    end;
                NewSta ->
                    NewSta
            end,
        STA = {sta, update_opts(OldSTA, NewSTA)},
        AP =
            case proplists:get_value(ap, OldConfig) of
                undefined -> [];
                Ap -> {ap, Ap}
            end,
        MDNS =
            case
                update_opts(
                    proplists:get_value(mdns, OldConfig, []),
                    proplists:get_value(mdns, NewConfig, [])
                )
            of
                [] -> [];
                MdnsCfg -> {mdns, MdnsCfg}
            end,
        SNTP =
            case
                update_opts(
                    proplists:get_value(sntp, OldConfig, []),
                    proplists:get_value(sntp, NewConfig, [])
                )
            of
                [] -> [];
                NewSntp -> {sntp, NewSntp}
            end,
        lists:flatten([STA, AP, MDNS, SNTP])
    catch
        error:Reason -> {error, Reason}
    end.

%% @private
update_opts(OldOpts, NewOpts) ->
    Old = proplists:to_map(OldOpts),
    New = proplists:to_map(NewOpts),
    NewMap = maps:merge(Old, New),
    proplists:from_map(NewMap).

%% @private
wait_for_ip(Timeout) ->
    receive
        connected ->
            wait_for_ip(Timeout);
        {ok, _IpInfo} = Started ->
            Started;
        disconnected ->
            {error, disconnected}
    after Timeout ->
        {error, timeout}
    end.

%% @private
wait_for_ap_started(Timeout) ->
    receive
        ap_started ->
            ok
    after Timeout ->
        {error, timeout}
    end.

%% @private
sta_disconnected_default_callback() ->
    sta_connect().

get_num_channels_timeout() ->
    case erlang:system_info(esp32_chip_info) of
        #{model := esp32_c5} ->
            {?DEVICE_2G5G_CHANNELS, 15000};
        _ ->
            {?DEVICE_2G_CHANNELS, 5000}
    end.
