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
% These values ate used to calculate the gen_server:call/3 timeout.
-define(DEVICE_TOTAL_CHANNELS, 14).
-define(GEN_RESPONSE_MS, 5000).
-define(MAX_SHORT_DWELL, 320).

-type ipv4_info() :: {
    IPAddress :: inet:ip4_address(), NetMask :: inet:ip4_address(), Gateway :: inet:ip4_address()
}.
-type ip_info() :: ipv4_info().
-type bssid_t() :: binary().

-type ssid_config() :: {ssid, string() | binary()}.
-type psk_config() :: {psk, string() | binary()}.

-type dhcp_hostname_config() :: {dhcp_hostname, string() | binary()}.
-type sta_connected_config() :: {connected, fun(() -> term())}.
-type sta_beacon_timeout_config() :: {beacon_timeout, fun(() -> term())}.
-type sta_disconnected_config() :: {disconnected, fun(() -> term())}.
-type sta_got_ip_config() :: {got_ip, fun((ip_info()) -> term())}.
-type sta_scan_config() ::
    {default_scan_results, 1..20}
    | {scan_dwell_ms, 1..1500}
    | scan_show_hidden
    | scan_passive.
-type sta_config_property() ::
    ssid_config()
    | psk_config()
    | dhcp_hostname_config()
    | sta_scan_config()
    | sta_connected_config()
    | sta_beacon_timeout_config()
    | sta_disconnected_config()
    | sta_got_ip_config().
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

-type scan_options() ::
    {results, 1..14 | 1..20}
    | {dwell, 1..1500}
    | show_hidden
    | passive.
%% The `results' key is used to set the maximum number of networks returned in the
%% networks list, and the `dwell' is used to set the dwell time (in milliseconds)
%% spent on each channel. The maximum number of results on esp32 classic is 14 and 20
%% on all other esp32 chips, due to network stack limitations. The option `show_hidden'
%% will also include hidden networks in the scan results. Default options are:
%% `[{results, 6}, {dwell, 120}]', if `passive' is used the default dwell time per
%% channel is 360 ms.

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
    | wpa3_ext_psk_mixed.

-type network_properties() :: [
    {rssi, dbm()} | [{authmode, auth_type()} | [{bssid, bssid_t()} | [{channel, wifi_channel()}]]]
].
%% A proplist of network properties with the keys: `rssi', `authmode' and `channel'

-record(state, {
    config :: network_config(),
    port :: port(),
    ref :: reference(),
    sta_ip_info :: ip_info(),
    mdns :: pid() | undefined
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
%%          connect to an AP endpoint in the background.  Specify callback
%%          functions to receive definitive
%%          information that the connection succeeded.  See the AtomVM Network
%%          FSM Programming Manual for more information.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config :: network_config()) -> {ok, pid()} | {error, Reason :: term()}.
start(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Config, []).

-spec start_link(Config :: network_config()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%-----------------------------------------------------------------------------
%% @returns ok, if the network interface was stopped, or {error, Reason} if
%%          a failure occurred.
%% @doc     Stop a network interface.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    gen_server:stop(?SERVER).

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

%% @param   Options is a list of `scan_options()'
%% @returns Scan result tuple, or {error, Reason} if a failure occurred.
%%
%% @doc     Scan for available WiFi networks.
%%
%% The network must first be started in sta or sta+ap mode before scanning for access points. While a
%% scan is in progress network traffic will be inhibited for clients connected to the esp32 access point
%% (in using ap+sta mode), but should not cause an active connection to be lost.  Espressif's documentation
%% recommends not exceeding 1500 ms per-chanel scan times or network connections may be lost, this is
%% enforced as a hard limit. The return is a tuple `{ok, Results}', where Results is a tuple with the number
%% of discovered networks and a list of networks, which may be shorter than the size of the discovered
%% networks if a smaller `MaxAPs' was used. The network tuples in the list consist of network name and a
%% proplist of network information:
%%
%%  `{ok, {NumberResults, [{SSID, [{rssi, DBm}, {authmode, Mode}, {bssid, BSSID}, {channel, Number}]}, ...]}}'
%%
%% For convenience `network_wifi_scan/0' may be used to scan with default options.  If the driver
%% had not been started before `network:wifi_scan/0,1' is used the network will be started in sta mode
%% with the `managed' option, all other options will use the default settings. This means after making
%% the first connection the driver will attempt to reconnect to the last network automatically and future
%% scans will not be possible because the wifi will always be associated with an access point, or in the
%% process of making a connection.
%%
%% Note: If a long dwell time is used, the return time for this function can be considerably longer
%% than the default gen_server timeout, especially when performing a passive scan. Passive scans
%% always use the full dwell time for each channel, active scans with a dwell time of more than 240
%% milliseconds will have a minimum dwell of 1/2 the maximum dwell time set by the `dwell' option.
%% The timeout for these longer scans is determined by the formula:
%% <pre>
%%     Timeout = (dwell * 14) + 5000
%% </pre>
%% That is the global ("world safe") maximum wifi channels multiplied by the dwell time with an
%% additional gen_server default timeout period added.  The actual number of channels scanned will be
%% determined by the country code, which currently does not have a configurable option, but is set to
%% `01' (world safe mode) and will automatically change to match the devices connection to an access
%% point.
%%
%% The default options may be configured by adding `sta_scan_config()' options to the
%% `sta_config()'.
%%
%% Warning: This feature is not yet available on the rp2040 platform.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec wifi_scan([Options :: scan_options(), ...]) ->
    {ok, {NetworksDiscovered :: 0..20, [{SSID :: string(), [ApInfo :: network_properties()]}, ...]}}
    | {error, Reason :: term()}.
wifi_scan(Options) ->
    Dwell = proplists:get_value(dwell, Options),
    case Dwell of
        undefined ->
            gen_server:call(maybe_start(?SERVER), {scan, Options});
        Millis when Millis =< ?MAX_SHORT_DWELL ->
            gen_server:call(maybe_start(?SERVER), {scan, Options});
        ChanDwellMs ->
            Timeout = (ChanDwellMs * ?DEVICE_TOTAL_CHANNELS) + ?GEN_RESPONSE_MS,
            gen_server:call(maybe_start(?SERVER), {scan, Options}, Timeout)
    end.

wifi_scan() ->
    Config = gen_server:call(maybe_start(?SERVER), get_config),
    Results = proplists:get_value(default_scan_results, proplists:get_value(sta, Config), 6),
    Dwell = proplists:get_value(scan_dwell_ms, proplists:get_value(sta, Config), 120),
    Hidden = proplists:get_value(scan_show_hidden, proplists:get_value(sta, Config), false),
    Passive = proplists:get_value(scan_passive, proplists:get_value(sta, Config), false),
    wifi_scan([{results, Results}, {dwell, Dwell}, {show_hidden, Hidden}, {passive, Passive}]).

%%
%% gen_server callbacks
%%

%% @hidden
init(Config) ->
    Port = get_port(),
    Ref = make_ref(),
    {ok, #state{config = Config, port = Port, ref = Ref}, {continue, start_port}}.

handle_continue(start_port, #state{config = Config, port = Port, ref = Ref} = State) ->
    Port ! {self(), Ref, {start, Config}},
    receive
        {Ref, ok} ->
            {noreply, State};
        {Ref, {error, Reason}} ->
            {stop, {start_port_failed, Reason}, State}
    end.

%% @private
wait_scan_results(Ref, From, State) ->
    receive
        {Ref, {error, _Reason} = ER} ->
            gen_server:reply(From, ER),
            {noreply, State#state{ref = Ref}};
        {Ref, Results} ->
            gen_server:reply(From, Results),
            {noreply, State#state{ref = Ref}};
        Any ->
            gen_server:reply(From, Any),
            {noreply, State#state{ref = Ref}}
    end.

%% @hidden
handle_call({scan, ScanOpts}, From, State) ->
    Ref = make_ref(),
    network_port ! {self(), Ref, {scan, ScanOpts}},
    wait_scan_results(Ref, From, State#state{ref = Ref});
handle_call(get_config, _From, #state{config = Config} = State) ->
    {reply, Config, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({Ref, sta_connected} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_connected_callback(Config),
    {noreply, State};
handle_info({Ref, sta_beacon_timeout} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_beacon_timeout_callback(Config),
    {noreply, State};
handle_info({Ref, sta_disconnected} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_disconnected_callback(Config),
    {noreply, State};
handle_info({Ref, {sta_got_ip, IpInfo}} = _Msg, #state{ref = Ref, config = Config} = State0) ->
    maybe_sta_got_ip_callback(Config, IpInfo),
    State1 = State0#state{sta_ip_info = IpInfo},
    State2 = maybe_start_mdns(State1),
    {noreply, State2};
handle_info({Ref, ap_started} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_ap_started_callback(Config),
    {noreply, State};
handle_info({Ref, {ap_sta_connected, Mac}} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_ap_sta_connected_callback(Config, Mac),
    {noreply, State};
handle_info({Ref, {ap_sta_disconnected, Mac}} = _Msg, #state{ref = Ref, config = Config} = State) ->
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

%%
%% Internal operations
%%

%% @private
maybe_sta_connected_callback(Config) ->
    maybe_callback0(connected, proplists:get_value(sta, Config)).

%% @private
maybe_sta_beacon_timeout_callback(Config) ->
    maybe_callback0(beacon_timeout, proplists:get_value(sta, Config)).

%% @private
maybe_sta_disconnected_callback(Config) ->
    maybe_callback0(disconnected, proplists:get_value(sta, Config)).

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
maybe_start(Server) ->
    case erlang:whereis(Server) of
        undefined ->
            StaConfig = [{sta, [managed]}],
            io:format("WARNING: Network driver not initialized, starting with config: ~p", [
                StaConfig
            ]),
            {ok, NewPid} = ?MODULE:start(StaConfig),
            NewPid;
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            {error, network_init_fail}
    end.
