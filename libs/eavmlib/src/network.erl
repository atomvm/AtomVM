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

%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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
    wait_for_ap/0, wait_for_ap/1, wait_for_ap/2
]).
-export([start/1, stop/0]).
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

-type dhcp_hostname_config() :: {dhcp_hostname, string() | binary()}.
-type sta_connected_config() :: {connected, fun(() -> term())}.
-type sta_disconnected_config() :: {disconnected, fun(() -> term())}.
-type sta_got_ip_config() :: {got_ip, fun((ip_info()) -> term())}.
-type sta_config_property() ::
    ssid_config()
    | psk_config()
    | dhcp_hostname_config()
    | sta_connected_config()
    | sta_disconnected_config()
    | sta_got_ip_config().
-type sta_config() :: {sta, [sta_config_property()]}.

-type mac() :: binary().
-type ap_ssid_hidden_config() :: {ap_ssid_hidden, boolean()}.
-type ap_max_connections_config() :: {ap_max_connections, non_neg_integer()}.
-type ap_started_config() :: {ap_started, fun(() -> term())}.
-type ap_sta_connected_config() :: {sta_connected, fun((mac()) -> term())}.
-type ap_sta_disconnected_config() :: {sta_disconnected, fun((mac()) -> term())}.
-type ap_sta_ip_assigned_config() :: {sta_ip_assigned, fun((ipv4_address()) -> term())}.
-type ap_config_property() ::
    ssid_config()
    | psk_config()
    | ap_ssid_hidden_config()
    | ap_max_connections_config()
    | ap_started_config()
    | ap_sta_connected_config()
    | ap_sta_disconnected_config()
    | ap_sta_ip_assigned_config().
-type ap_config() :: {sta, [ap_config_property()]}.

-type sntp_config_property() :: {host, string() | binary()}.
-type sntp_config() :: {sntp, [sntp_config_property()]}.

-type network_config() :: [sta_config() | ap_config() | sntp_config()].

-record(state, {
    config :: network_config(),
    port :: port(),
    ref :: reference(),
    sta_ip_info :: ip_info()
}).

%%-----------------------------------------------------------------------------
%% @doc     Equivalent to wait_for_sta(15000).
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_sta() -> {ok, ip_info()} | {error, Reason :: term()}.
wait_for_sta() ->
    wait_for_sta(15000).

%%-----------------------------------------------------------------------------
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
%% @returns {ok, IpInfo}, if the network_fsm was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Start a network_fsm in station mode and wait for a connection to be established
%%
%%          This function will start a network_fsm in station mode, and will wait
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
        ok ->
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
%% @doc     Equivalent to wait_for_ap([], Timeout) or wait_for_ap(StaConfig, 15000).
%% @end
%%-----------------------------------------------------------------------------
-spec wait_for_ap(TimeoutOrStaConfig :: non_neg_integer() | [ap_config_property()]) ->
    ok | {error, Reason :: term()}.
wait_for_ap(Timeout) when is_integer(Timeout) ->
    wait_for_ap([], Timeout);
wait_for_ap(ApConfig) when is_list(ApConfig) ->
    wait_for_ap(ApConfig, 15000).

%%-----------------------------------------------------------------------------
%% @param   ApConfig The AP network configuration
%% @param   Timeout amount of time in milliseconds to wait for a connection
%% @returns ok, when the nework_fsm has started the AP, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Start a network_fsm in access point mode and wait the AP to be
%%          up and running
%%
%%          This function will start a network_fsm in AP mode, and will wait
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
        ok ->
            wait_for_ap_started(Timeout);
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Config The network configuration
%% @returns ok, if the network_fsm was started, or {error, Reason} if
%%          a failure occurred (e.g., due to malformed network configuration).
%% @doc     Start a network_fsm.
%%
%%          This function will start a network_fsm, which will attempt to
%%          connect to an AP endpoint in the background.  Specify callback
%%          functions to receive definitive
%%          information that the connection succeeded.  See the AtomVM Network
%%          FSM Programming Manual for more information.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Config :: network_config()) -> ok | {error, Reason :: term()}.
start(Config) ->
    case gen_server:start({local, ?MODULE}, ?MODULE, Config, []) of
        {ok, Pid} ->
            gen_server:call(Pid, start);
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @returns ok, if the network_fsm was stopped, or {error, Reason} if
%%          a failure occurred.
%% @doc     Stop a network_fsm.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    gen_server:stop(?SERVER).

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
    DriverConfig = get_driver_config(Config),
    Port ! {self(), Ref, {start, DriverConfig}},
    wait_start_reply(Ref, From, Port, State);
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
            {stop, {start_failed, Reason}, ER, State}
    end.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({Ref, sta_connected} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_connected_callback(Config),
    {noreply, State};
handle_info({Ref, sta_disconnected} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_disconnected_callback(Config),
    {noreply, State};
handle_info({Ref, {sta_got_ip, IpInfo}} = _Msg, #state{ref = Ref, config = Config} = State) ->
    maybe_sta_got_ip_callback(Config, IpInfo),
    {noreply, State#state{sta_ip_info = IpInfo}};
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
handle_info(Msg, State) ->
    io:format("Received spurious message ~p~n", [Msg]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% Internal operations
%%

%% @private
get_driver_config(Config) ->
    Config1 =
        case proplists:get_value(sta, Config) of
            undefined ->
                Config;
            StaConfig ->
                NewStaConfig1 = maybe_add_nvs_entry(ssid, StaConfig, sta_ssid),
                NewStaConfig2 = maybe_add_nvs_entry(psk, NewStaConfig1, sta_psk),
                [{sta, NewStaConfig2} | lists:keydelete(sta, 1, Config)]
        end,
    Config2 =
        case proplists:get_value(ap, Config1) of
            undefined ->
                Config1;
            ApConfig ->
                NewApConfig1 = maybe_add_nvs_entry(ssid, ApConfig, ap_ssid),
                NewApConfig2 = maybe_add_nvs_entry(psk, NewApConfig1, ap_psk),
                [{ap, NewApConfig2} | lists:keydelete(ap, 1, Config1)]
        end,
    Config2.

%% @private
maybe_add_nvs_entry(Key, List, NVSKey) ->
    case proplists:get_value(Key, List) of
        undefined ->
            case esp:nvs_get_binary(atomvm, NVSKey) of
                undefined ->
                    List;
                NVSValue ->
                    [{Key, NVSValue} | List]
            end;
        _Value ->
            List
    end.

%% @private
maybe_sta_connected_callback(Config) ->
    maybe_callback0(connected, proplists:get_value(sta, Config)).

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
maybe_callback0(_Key, undefined) ->
    ok;
maybe_callback0(Key, Config) ->
    case proplists:get_value(Key, Config) of
        undefined -> ok;
        Pid when is_pid(Pid) -> Pid ! Key;
        Fun -> Fun()
    end.

%% @private
maybe_callback1(_KeyArg, undefined) ->
    ok;
maybe_callback1({Key, Arg} = Msg, Config) ->
    case proplists:get_value(Key, Config) of
        undefined -> ok;
        Pid when is_pid(Pid) -> Pid ! Msg;
        Fun -> Fun(Arg)
    end.

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
