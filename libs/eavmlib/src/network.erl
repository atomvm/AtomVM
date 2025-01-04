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
    sta_status/0
]).
-export([start/1, start_link/1, stop/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).

-type octet() :: 0..255.
-type ipv4_address() :: {octet(), octet(), octet(), octet()}.
-type ipv4_info() :: {
    IPAddress :: ipv4_address(), NetMask :: ipv4_address(), Gateway :: ipv4_address()
}.
-type ip_info() :: ipv4_info().

-type ssid_config() :: {ssid, string() | binary()}.
-type psk_config() :: {psk, string() | binary()}.
-type app_managed_config() :: managed | {managed, boolean()}.
%% Setting `{managed, true}' or including the atom `managed' in the `sta_config()' will signal to
%% the driver that the connections are managed in the user application, allowing to start the
%% driver in STA (or AP+STA) mode, but delay starting a connection by omitting `ssid' and `psk'
%% configuration values. When using this mode of operation applications may want to provide an
%% `sta_disconnected_config()' to replace the default callback, which attempts to reconnect to the
%% last network, and instead scan for available networks, or use some other means of determining
%% when, and which network to connect to.

-type dhcp_hostname_config() :: {dhcp_hostname, string() | binary()}.
-type sta_connected_config() :: {connected, fun(() -> term())}.
-type sta_beacon_timeout_config() :: {beacon_timeout, fun(() -> term())}.
-type sta_disconnected_config() :: {disconnected, fun(() -> term())}.
%% If no callback is configured the default behavior when the connection to an access point is
%% lost is to attempt to reconnect. If a callback is provided these automatic reconnections will
%% no longer occur, and the application must use `network:sta_connect/0' to reconnect to the last
%% access point, or use `network:sta_connect/1' to connect to a new access point in a manner
%% determined by the application.

-type sta_got_ip_config() :: {got_ip, fun((ip_info()) -> term())}.
-type sta_config_property() ::
    app_managed_config()
    | ssid_config()
    | psk_config()
    | dhcp_hostname_config()
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
-type ap_sta_ip_assigned_config() :: {sta_ip_assigned, fun((ipv4_address()) -> term())}.
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

-type network_config() :: [sta_config() | ap_config() | sntp_config()].

-type db() :: integer().
-type sta_status() ::
    associated | connected | connecting | degraded | disconnected | disconnecting | undefined.

-record(state, {
    config :: network_config(),
    port :: port(),
    ref :: reference(),
    sta_ip_info :: ip_info(),
    sta_state :: sta_status()
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
%%          option us used the driver will be started, but the connection will be
%%          delayed until `network:connect/0,1' is used to start the connection.
%%          Specify callback functions to receive definitive information that the
%%          connection succeeded; specify a `sta_disconnected_config()' in the
%%          `sta_config()' to manage re-connections in the application, rather than
%%          the default automatic attempt to reconnect until a connection is
%%          reestablished.
%%
%%          See the AtomVM Network Programming Manual for more information.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config :: network_config()) -> {ok, pid()} | {error, Reason :: term()}.
start(Config) ->
    case gen_server:start({local, ?MODULE}, ?MODULE, Config, []) of
        {ok, Pid} = R ->
            case gen_server:call(Pid, start) of
                ok ->
                    R;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec start_link(Config :: network_config()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Config) ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []) of
        {ok, Pid} = R ->
            case gen_server:call(Pid, start) of
                ok ->
                    R;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

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
    gen_server:call(?SERVER, halt_sta).

%%-----------------------------------------------------------------------------
%% @param   Config The new station mode mode network configuration, if no
%%          configuration is given the driver will attempt to reconnect to
%%          the last access point it configured to use.
%% @returns ok, if the network interface was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Connect to a new access point after the network driver has been started.
%%
%%          This function will attempt to connect to an AP endpoint in the
%%          background.
%%
%%          The `dhcp_hostname' and sntp server configuration can be changed,
%%          but any callback settings included in the configuration will be
%%          ignored, callbacks must be configured when the driver is started
%%          with `network:start/1'.
%% @end
%%-----------------------------------------------------------------------------
-spec sta_connect(Config :: network_config()) -> ok | {error, Reason :: term()}.
sta_connect(Config) ->
    gen_server:call(?SERVER, {connect, Config}).

%%-----------------------------------------------------------------------------
%% @returns ok, if the network interface was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Reconnect to an access point after a network disconnection.
%%
%%          This function will attempt to reconnect, in the background, to the
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
    gen_server:stop(?SERVER).

%%-----------------------------------------------------------------------------
%% @returns {ok, Rssi} in decibels, or {error, Reason}.
%%
%% @doc     Get the rssi information of AP to which the device is associated with.
%% @end
%%-----------------------------------------------------------------------------
-spec sta_rssi() -> {ok, Rssi :: db()} | {error, Reason :: term()}.
sta_rssi() ->
    Port = get_port(),
    Ref = make_ref(),
    Port ! {self(), Ref, rssi},
    receive
        {Ref, {error, Reason}} -> {error, Reason};
        {Ref, {rssi, Rssi}} -> {ok, Rssi};
        Other -> {error, Other}
    end.

%%-----------------------------------------------------------------------------
%% @returns ConnectionState :: sta_status().
%%
%% @doc Get the connection status of the sta interface.
%%
%% Results will be one of: `associated', `connected', `connecting', `degraded',
%% `disconnected', `disconnecting', or `undefined'. The state `associated' indicates
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

%%
%% gen_server callbacks
%%

%% @hidden
init(Config) ->
    {ok, #state{config = Config}}.

%% @hidden
handle_call(start, From, #state{config = Config} = State) ->
    Port = get_port(),
    Ref = make_ref(),
    Port ! {self(), Ref, {start, Config}},
    case proplists:get_value(sta, Config) of
        undefined ->
            wait_start_reply(Ref, From, Port, State);
        STA ->
            case proplists:get_value(managed, STA) of
                false ->
                    wait_start_reply(Ref, From, Port, State#state{sta_state = connecting});
                true ->
                    wait_start_reply(Ref, From, Port, State#state{sta_state = disconnected})
            end
    end;
handle_call(halt_sta, _From, #state{ref = Ref} = State) ->
    network_port ! {self(), Ref, halt_sta},
    wait_halt_sta_reply(Ref, State#state{sta_state = disconnecting});
handle_call(connect, _From, #state{config = Config, ref = Ref} = State) ->
    network_port ! {self(), Ref, {connect, Config}},
    wait_connect_reply(Ref, Config, State#state{sta_state = connecting});
handle_call({connect, Config}, _From, #state{config = OldConfig, ref = Ref} = State) ->
    NewConfig = update_config(OldConfig, Config),
    network_port ! {self(), Ref, {connect, NewConfig}},
    wait_connect_reply(Ref, NewConfig, State#state{sta_state = connecting});
handle_call(sta_status, _From, State) ->
    {reply, State#state.sta_state, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

%% @private
wait_start_reply(Ref, From, Port, State) ->
    receive
        {Ref, ok} ->
            gen_server:reply(From, ok),
            {noreply, State#state{port = Port, ref = Ref}};
        {Ref, {error, Reason} = ER} ->
            gen_server:reply(From, {error, Reason}),
            {stop, {start_failed, Reason}, ER, State#state{sta_state = disconnected}}
    end.

%% @private
wait_connect_reply(Ref, NewConfig, State) ->
    receive
        {Ref, ok} ->
            {reply, ok, State#state{config = NewConfig}};
        {Ref, {error, _Reason} = ER} ->
            {reply, ER, State#state{sta_state = disconnected}}
    end.

wait_halt_sta_reply(Ref, State) ->
    receive
        {Ref, ok} ->
            {reply, ok, State#state{sta_state = disconnected}};
        {Ref, {error, _Reason} = Error} ->
            {reply, Error, State}
    end.

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
handle_info({Ref, {sta_got_ip, IpInfo}} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_got_ip_callback(Config, IpInfo),
    {noreply, State#state{sta_ip_info = IpInfo, sta_state = connected}};
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
handle_info(Msg, State) ->
    io:format("Received spurious message ~p~n", [Msg]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    Ref = make_ref(),
    network_port ! {?SERVER, Ref, stop},
    ok.

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
    maybe_callback0(
        disconnected, proplists:get_value(sta, Config, fun sta_disconnected_default_callback/0)
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
update_config(OldConfig, NewConfig) ->
    OldSta = proplists:get_value(sta, OldConfig),
    case proplists:get_value(sta, NewConfig, undefined) of
        [{ssid, SSID}, {psk, PSK}] ->
            ok;
        undefined ->
            SSID = proplists:get_value(ssid, NewConfig),
            PSK = proplists:get_value(psk, NewConfig)
    end,
    SntpConfig = proplists:get_value(sntp, NewConfig, proplists:get_value(sntp, OldConfig)),
    ApConfig = proplists:get_value(ap, OldConfig),
    Hostname = proplists:get_value(
        dhcp_hostname, NewConfig, proplists:get_value(dhcp_hostname, OldConfig)
    ),
    case Hostname of
        undefined ->
            TempList0 = OldSta;
        Name ->
            TempList0 = lists:keyreplace(dhcp_hostname, 1, OldSta, {dhcp_hostname, Name})
    end,
    TempList1 = lists:keyreplace(ssid, 1, TempList0, {ssid, SSID}),
    StaConfig = lists:keyreplace(psk, 1, TempList1, {psk, PSK}),
    case ApConfig of
        undefined ->
            case SntpConfig of
                undefined -> UpdatedConfig = [{sta, StaConfig}];
                _ -> UpdatedConfig = [{sta, StaConfig}, {sntp, SntpConfig}]
            end;
        _ ->
            case SntpConfig of
                undefined -> UpdatedConfig = [{ap, ApConfig}, {sta, StaConfig}];
                _ -> UpdatedConfig = [{ap, ApConfig}, {sta, StaConfig}, {sntp, SntpConfig}]
            end
    end,
    UpdatedConfig.

%% @private
get_port() ->
    case whereis(network_port) of
        undefined ->
            open_port();
        Pid ->
            Pid
    end.

%% @private
open_port() ->
    Pid = erlang:open_port({spawn, "network"}, []),
    %Pid = spawn(?MODULE, simulation_loop, []),
    erlang:register(network_port, Pid),
    Pid.

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
