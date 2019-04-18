%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% @doc WARNING: Interfaces around management of networking are under
%% revision and may change without notice.
%%
-module(network_fsm).

-export([start/1, stop/0]).
-export([init/1, initial/3, wait_for_sta_connected/3, wait_for_sta_got_ip/3, sta_got_ip/3, terminate/3]).
-export([simulation_loop/0]).

-include("estdlib.hrl").

-define(SERVER, ?MODULE).

-type octet() :: 0..255.
-type ipv4_address() :: {octet(), octet(), octet(), octet()}.
-type ipv4_info() :: {IPAddress::ipv4_address(), NetMask::ipv4_address(), Gateway::ipv4_address()}.
-type ip_info() :: ipv4_info().

-type ssid_config() :: {ssid, string()}.
-type psk_config() :: {psk, string()}.
-type connected_config() :: {connected, fun(() -> term())}.
-type disconnected_config() :: {disconnected, fun(() -> term())}.
-type got_ip_config() :: {got_ip, fun((ip_info()) -> term())}.
-type sta_config_property() :: ssid_config() | psk_config() | connected_config() | disconnected_config() | got_ip_config().
-type sta_config() :: {sta, [sta_config_property()]}.
-type mode_config() :: sta_config().
-type network_config() :: [mode_config()].

-include("logger.hrl").

-record(data, {
    config :: network_config(),
    port :: port(),
    ref :: reference(),
    sta_ip_info :: ip_info()
}).

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
-spec start(Config::network_config()) -> ok | {error, Reason::term()}.
start(Config) ->
    ?LOG_DEBUG({start, Config}),
    case ?GEN_STATEM:start({local, ?MODULE}, ?MODULE, Config, []) of
        {ok, Pid} ->
            ?GEN_STATEM:call(Pid, start);
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @returns ok, if the network_fsm was stopped, or {error, Reason} if
%%          a failure occurred.
%% @doc     Stop a network_fsm.
%% @end
%%-----------------------------------------------------------------------------
-spec stop() -> ok | {error, Reason::term()}.
stop() ->
    ?GEN_STATEM:stop(?SERVER).

%%
%% gen_statem callbacks
%%

%% @hidden
init(Config) ->
   {ok, initial, #data{config=Config}}.

%%-----------------------------------------------------------------------------
%% initial state
%%-----------------------------------------------------------------------------

%% @hidden
initial({call, From}, start, #data{config=Config} = Data) ->
    Port = get_port(),
    Ref = make_ref(),
    case start_network(Port, Ref, get_port_config(Config)) of
        ok ->
            ?GEN_STATEM:reply(From, ok),
            {next_state, wait_for_sta_connected, Data#data{port=Port, ref=Ref}, get_timeout_actions(Config)};
        {error, Reason} ->
            ?GEN_STATEM:reply(From, {error, Reason}),
            {stop, {start_failed, Reason}}
    end.


%%-----------------------------------------------------------------------------
%% wait_for_sta_connected state
%%-----------------------------------------------------------------------------

wait_for_sta_connected(info, {Ref, sta_connected} = Msg, #data{ref=Ref, config=Config} = Data) ->
    ?LOG_DEBUG({wait_for_sta_connected, [info, Msg, Data]}),
    maybe_sta_connected_callback(Config),
    {next_state, wait_for_sta_got_ip, Data};

%% disconnected; likely an authn failure
wait_for_sta_connected(info, {Ref, sta_disconnected} = Msg, #data{ref=Ref, config=Config} = Data) ->
    ?LOG_DEBUG({wait_for_sta_connected, [info, Msg, Data]}),
    maybe_sta_disconnected_callback(Config),
    {next_state, wait_for_sta_connected, Data};

% catch-all
wait_for_sta_connected(EventType, Msg, Data) ->
    ?LOG_DEBUG({wait_for_sta_connected, [EventType, Msg, Data]}),
    {stop, {unexpected_msg, wait_for_sta_connected, EventType, Msg}, Data}.


%%-----------------------------------------------------------------------------
%% wait_for_sta_got_ip state
%%-----------------------------------------------------------------------------

%% disconnected; likely an authn failure
wait_for_sta_got_ip(info, {Ref, sta_disconnected} = Msg, #data{ref=Ref, config=Config} = Data) ->
    ?LOG_DEBUG({wait_for_sta_got_ip, [info, Msg, Data]}),
    maybe_sta_disconnected_callback(Config),
    {next_state, wait_for_sta_connected, Data};

%% got an ip!
wait_for_sta_got_ip(info, {Ref, {sta_got_ip, IpInfo}} = Msg, #data{ref=Ref, config=Config} = Data) ->
    ?LOG_DEBUG({wait_for_sta_got_ip, [info, Msg, Data]}),
    maybe_sta_got_ip_callback(Config, IpInfo),
    {next_state, sta_got_ip, Data#data{sta_ip_info=IpInfo}};

% catch-all
wait_for_sta_got_ip(EventType, Msg, Data) ->
    ?LOG_DEBUG({wait_for_sta_got_ip, [EventType, Msg, Data]}),
    {stop, {unexpected_msg, wait_for_sta_got_ip, EventType, Msg}, Data}.


%%-----------------------------------------------------------------------------
%% sta_got_ip state
%%-----------------------------------------------------------------------------

%% @hidden
sta_got_ip({call, From}, get_ip, #data{sta_ip_info=IpInfo} = Data) ->
    ?LOG_DEBUG({sta_got_ip, [{call, From}, get_ip, Data]}),
    {next_state, sta_got_ip, Data, [{reply, From, {ok, IpInfo}}]};

%% disconnected; likely an authn failure
sta_got_ip(info, {Ref, sta_disconnected} = Msg, #data{ref=Ref, config=Config} = Data) ->
    ?LOG_DEBUG({sta_got_ip, [info, Msg, Data]}),
    maybe_sta_disconnected_callback(Config),
    {next_state, wait_for_sta_connected, Data#data{sta_ip_info=undefined}};

% catch-all
sta_got_ip(EventType, Msg, Data) ->
    ?LOG_DEBUG({sta_got_ip, [EventType, Msg, Data]}),
    {stop, {unexpected_msg, sta_got_ip, EventType, Msg}, Data}.


%% @hidden
terminate(_Reason, _StateName, _Data) ->
    ok.

%%
%% Internal operations
%%

%% @private
get_port_config(Config) ->
    Sta = case ?PROPLISTS:get_value(sta, Config) of
        undefined -> [];
        StaConfig ->
            PasswordConfig = case ?PROPLISTS:get_value(psk, StaConfig) of
                undefined ->
                    [];
                Value ->
                    [{psk, Value}]
            end,
            {sta, [{ssid, ?PROPLISTS:get_value(ssid, StaConfig)} | PasswordConfig]}
    end,
    [Sta | ?LISTS:keydelete(sta, 1, Config)].

get_timeout_actions(Config) ->
    case ?PROPLISTS:get_value(timeout, Config) of
        undefined ->
            [];
        Timeout ->
            [{state_timeout, Timeout, error}]
    end.

maybe_sta_connected_callback(Config) ->
    maybe_callback0(connected, ?PROPLISTS:get_value(sta, Config)).

maybe_sta_disconnected_callback(Config) ->
    maybe_callback0(disconnected, ?PROPLISTS:get_value(sta, Config)).

maybe_sta_got_ip_callback(Config, IpInfo) ->
    maybe_callback1({got_ip, IpInfo}, ?PROPLISTS:get_value(sta, Config)).


maybe_callback0(_Key, undefined) ->
    ok;
maybe_callback0(Key, Config) ->
    case ?PROPLISTS:get_value(Key, Config) of
        undefined ->    ok;
        Pid when is_pid(Pid) -> Pid ! Key;
        Fun ->          Fun()
    end.

maybe_callback1(_KeyArg, undefined) ->
    ok;
maybe_callback1({Key, Arg} = Msg, Config) ->
    case ?PROPLISTS:get_value(Key, Config) of
        undefined ->    ok;
        Pid when is_pid(Pid) -> Pid ! Msg;
        Fun ->          Fun(Arg)
    end.

%% @private
start_network(Port, Ref, Config) ->
    ?LOG_DEBUG({sending_to_port, Port, {self(), Ref, {start, Config}}}),
    Port ! {self(), Ref, {start, Config}},
    receive
        {Ref, Msg} -> Msg
    end.

%% @private
get_port() ->
    case whereis(network_port) of
        undefined ->
            open_port();
        Pid -> Pid
    end.

%% @private
open_port() ->
    Pid = erlang:open_port({spawn, "network"}, []),
    %Pid = spawn(?MODULE, simulation_loop, []),
    erlang:register(network_port, Pid),
    Pid.

simulation_loop() ->
    receive
        {Pid, Ref, {start, _Config}} ->
            IpInfo = {{192,168,1,236}, {255,255,255,0}, {192,168,1,1}},
            % timer_manager:send_after(424, Pid, {Ref, sta_connected}),
            % timer_manager:send_after(737, Pid, {Ref, {sta_got_ip, IpInfo}}),
            receive after 424 ->
                Pid ! {Ref, sta_connected}
            end,
            receive after 315 ->
                Pid ! {Ref, {sta_got_ip, IpInfo}}
            end,
            ok
    end,
    simulation_loop().
