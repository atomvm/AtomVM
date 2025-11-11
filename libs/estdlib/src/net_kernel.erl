%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP net_kernel interface.
%%
%% This module implements a strict subset of the Erlang/OTP net_kernel
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(net_kernel).

-behaviour(gen_server).

% Public API
-export([
    start/2,
    stop/0,
    get_state/0,
    epmd_module/0,
    get_net_ticktime/0
]).

% supervised callback
-export([
    start_link/1
]).

% Interface to dist_util
-export([
    mark_pending/4,
    mark_nodeup/4,
    get_cookie/0,
    get_cookie/1,
    set_cookie/1,
    set_cookie/2
]).

% gen_server interface
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SETUPTIME, 7000).
-define(NET_TICK_INTENSITY, 4).
-define(NET_TICK_TIME, 60).

%%-----------------------------------------------------------------------------
%% @doc Start erlang distribution
%% @end
%% @param Name name of the node
%% @param Options options for distribution. Supported options are:
%% - `name_domain' : whether name should be short or long
%% - `proto_dist' : the module used for distribution (e.g. `socket_dist')
%% - `dist_listen_min' : defines the min port range for the listener socket.
%% - `dist_listen_max' : defines the max port range for the listener socket.
%%-----------------------------------------------------------------------------
-spec start(atom(), map()) -> {ok, pid()}.
start(Name, Options0) when is_atom(Name) andalso is_map(Options0) ->
    ok = maps:foreach(
        fun(Key, Val) ->
            case Key of
                name_domain when Val =:= shortnames orelse Val =:= longnames -> ok;
                proto_dist when is_atom(Val) -> ok;
                dist_listen_min when is_integer(Val) -> ok;
                dist_listen_max when is_integer(Val) -> ok;
                _ -> error({invalid_option, Key, Val}, [Name, Options0])
            end
        end,
        Options0
    ),
    % Check that if one of dist_listen_min and dist_listen_max are configured, both are configured.
    % And verify dist_listen_max is larger or equal to dist_listen_min.
    ok =
        case {maps:is_key(dist_listen_min, Options0), maps:is_key(dist_listen_max, Options0)} of
            {true, false} ->
                error(missing_dist_listen_max, [Name, Options0]);
            {false, true} ->
                error(missing_dist_listen_min, [Name, Options0]);
            {true, true} ->
                Min = maps:get(dist_listen_min, Options0),
                Max = maps:get(dist_listen_max, Options0),
                if
                    Min > Max -> error(invalid_port_range, [Name, Options0]);
                    true -> ok
                end;
            _ ->
                ok
        end,
    Options1 = Options0#{name => Name},
    Options2 = split_name(Options1),
    net_kernel_sup:start(Options2);
start(Name, Options) when is_map(Options) ->
    error(invalid_name, [Name, Options]);
start(Name, Options) ->
    error(invalid_options, [Name, Options]).

%%-----------------------------------------------------------------------------
%% @doc Stop erlang distribution
%% @end
%%-----------------------------------------------------------------------------
stop() ->
    net_kernel_sup:stop().

%%-----------------------------------------------------------------------------
%% @doc Get state of erlang distribution
%% @end
%% @return a map describing state of distribution
%%-----------------------------------------------------------------------------
-spec get_state() -> map().
get_state() ->
    try
        gen_server:call(net_kernel, get_state, infinity)
    catch
        exit:{Reason, _} when
            Reason =:= noproc;
            Reason =:= shutdown;
            Reason =:= killed
        ->
            #{started => no}
    end.

%%-----------------------------------------------------------------------------
%% @doc Get the epmd client module implementation.
%% @end
%% @return the module to use to connect to epmd daemon
%%-----------------------------------------------------------------------------
-spec epmd_module() -> module().
epmd_module() ->
    erl_epmd.

%%-----------------------------------------------------------------------------
%% @doc Get the current net tick time
%% @end
%% @return the net tick time in seconds
%%-----------------------------------------------------------------------------
-spec get_net_ticktime() -> pos_integer().
get_net_ticktime() ->
    ?NET_TICK_TIME.

%% @hidden
-spec mark_pending(pid(), node(), node(), pid()) -> ok | {ok_simultaneous, pid()} | nok | alive.
mark_pending(Kernel, ThisNode, OtherNode, ConnPid) ->
    gen_server:call(Kernel, {mark_pending, ThisNode, OtherNode, ConnPid}).

%% @hidden
-spec mark_nodeup(pid(), node(), term(), pid()) -> {ok, pos_integer()} | {error, any()}.
mark_nodeup(Kernel, OtherNode, OtherAddress, ConnPid) ->
    gen_server:call(Kernel, {mark_nodeup, OtherNode, OtherAddress, ConnPid}).

%% @hidden
-spec get_cookie() -> binary().
get_cookie() ->
    gen_server:call(net_kernel, get_cookie, infinity).

%% @hidden
-spec get_cookie(node()) -> binary().
get_cookie(Node) ->
    gen_server:call(net_kernel, {get_cookie, Node}, infinity).

%% @hidden
-spec set_cookie(binary()) -> ok.
set_cookie(Cookie) ->
    gen_server:call(net_kernel, {set_cookie, Cookie}, infinity).

%% @hidden
-spec set_cookie(node(), binary()) -> ok.
set_cookie(Node, Cookie) ->
    gen_server:call(net_kernel, {set_cookie, Node, Cookie}, infinity).

%% @hidden
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%%-----------------------------------------------------------------------------
-record(state, {
    ticker :: pid(),
    longnames :: boolean(),
    node :: node(),
    listen :: any(),
    accept_pid :: pid(),
    proto_dist :: module(),
    connections :: map(),
    cookies :: map(),
    cookie :: binary()
}).

%% @hidden
init(Options) ->
    process_flag(trap_exit, true),
    LongNames = maps:get(name_domain, Options, longnames) =:= longnames,
    ProtoDist = maps:get(proto_dist, Options, socket_dist),
    DistPortMin = maps:get(dist_listen_min, Options, 0),
    DistPortMax = maps:get(dist_listen_max, Options, 0),
    Name = maps:get(name, Options),
    Node = maps:get(node, Options),
    Cookie = crypto:strong_rand_bytes(16),
    TickInterval = (?NET_TICK_TIME * 1000) div ?NET_TICK_INTENSITY,
    Self = self(),
    Ticker = spawn_link(fun() -> ticker(Self, TickInterval) end),
    % Try ports in range until one succeeds
    case try_listen_ports(ProtoDist, Name, DistPortMin, DistPortMax) of
        {ok, {Listen, _Address, Creation}} ->
            true = erlang:setnode(Node, Creation),
            AcceptPid = ProtoDist:accept(Listen),
            {ok, #state{
                ticker = Ticker,
                longnames = LongNames,
                node = Node,
                listen = Listen,
                accept_pid = AcceptPid,
                proto_dist = ProtoDist,
                connections = maps:new(),
                cookies = maps:new(),
                cookie = Cookie
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

%% @hidden
try_listen_ports(ProtoDist, Name, DistPortMin, DistPortMax) ->
    try_listen_ports(ProtoDist, Name, DistPortMin, DistPortMax, DistPortMin).

try_listen_ports(_ProtoDist, _Name, _DistPortMin, DistPortMax, Port) when Port > DistPortMax ->
    {error, no_port_available};
try_listen_ports(ProtoDist, Name, DistPortMin, DistPortMax, Port) ->
    case ProtoDist:listen(Name, Port) of
        {ok, _} = Success -> Success;
        {error, _} -> try_listen_ports(ProtoDist, Name, DistPortMin, DistPortMax, Port + 1)
    end.

%% @hidden
handle_call(get_state, _From, #state{longnames = Longnames} = State) ->
    NameDomain =
        case Longnames of
            true -> longnames;
            false -> shortnames
        end,
    Result = #{
        started => dynamic,
        name_type => static,
        name => node(),
        name_domain => NameDomain
    },
    {reply, Result, State};
handle_call(
    {mark_pending, ThisNode, OtherNode, ConnPid}, _From, #state{connections = Connections} = State0
) ->
    case maps:find(OtherNode, Connections) of
        error ->
            {reply, ok, State0#state{
                connections = maps:put(OtherNode, {pending, ConnPid, undefined}, Connections)
            }};
        {ok, {pending, undefined, DHandle}} ->
            {reply, ok, State0#state{
                connections = maps:put(OtherNode, {pending, ConnPid, DHandle}, Connections)
            }};
        {ok, {pending, OtherConnPid, DHandle}} when OtherNode > ThisNode ->
            {reply, {ok_simultaneous, OtherConnPid}, State0#state{
                connections = maps:update(OtherNode, {pending, ConnPid, DHandle}, Connections)
            }};
        {ok, {pending, _OtherConnPid, _DHandle}} ->
            {reply, nok, State0};
        {ok, {alive, _ConnPid, _Address}} ->
            {reply, alive, State0}
    end;
handle_call(
    {mark_nodeup, OtherNode, OtherAddress, ConnPid},
    _From,
    #state{connections = Connections0} = State0
) ->
    Connections1 = maps:update(OtherNode, {alive, ConnPid, OtherAddress}, Connections0),
    State1 = State0#state{connections = Connections1},
    {reply, {ok, ?NET_TICK_INTENSITY}, State1};
handle_call(get_cookie, _From, #state{cookie = Cookie} = State) ->
    {reply, Cookie, State};
handle_call({get_cookie, Node}, _From, #state{cookie = Cookie, cookies = Cookies} = State) ->
    case maps:find(Node, Cookies) of
        error -> {reply, Cookie, State};
        {ok, NodeCookie} -> {reply, NodeCookie, State}
    end;
handle_call({set_cookie, Cookie}, _From, #state{} = State) ->
    {reply, ok, State#state{cookie = Cookie}};
handle_call({set_cookie, Node, Cookie}, _From, #state{cookies = Cookies0} = State) ->
    if
        Node =:= node() ->
            {reply, ok, State#state{cookie = Cookie}};
        true ->
            Cookies1 = maps:put(Node, Cookie, Cookies0),
            {reply, ok, State#state{cookies = Cookies1}}
    end;
handle_call({is_auth, _Node}, _From, State) ->
    {reply, yes, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info({accept, AcceptPid, SocketPid, inet, tcp}, #state{proto_dist = ProtoDist} = State) ->
    Pid = ProtoDist:accept_connection(AcceptPid, SocketPid, State#state.node, [], ?SETUPTIME),
    AcceptPid ! {self(), controller, Pid},
    {noreply, State};
handle_info(tick, #state{connections = Connections} = State) ->
    maps:fold(
        fun(_Node, Status, ok) ->
            case Status of
                {alive, ConnPid, _Address} ->
                    ConnPid ! {self(), tick};
                _ ->
                    ok
            end
        end,
        ok,
        Connections
    ),
    {noreply, State};
handle_info({'EXIT', Ticker, _Reason}, #state{ticker = Ticker} = State) ->
    TickInterval = (?NET_TICK_TIME * 1000) div ?NET_TICK_INTENSITY,
    Self = self(),
    NewTicker = spawn_link(fun() -> ticker(Self, TickInterval) end),
    {noreply, State#state{ticker = NewTicker}};
handle_info(
    {'EXIT', AcceptPid, _Reason},
    #state{accept_pid = AcceptPid, listen = Listen, proto_dist = ProtoDist} = State
) ->
    NewAcceptPid = ProtoDist:accept(Listen),
    {noreply, State#state{accept_pid = NewAcceptPid}};
handle_info({'EXIT', Pid, _Reason}, #state{connections = Connections} = State) ->
    NewConnections = maps:filter(
        fun(_Node, Status) ->
            case Status of
                {alive, Pid, _Address} -> false;
                {pending, Pid, _DHandle} -> false;
                _ -> true
            end
        end,
        Connections
    ),
    {noreply, State#state{connections = NewConnections}};
handle_info(
    {connect, OtherNode, DHandle},
    #state{connections = Connections, node = MyNode, longnames = Longnames, proto_dist = ProtoDist} =
        State
) ->
    % ensure DHandle is not garbage collected until setup failed or succeeded
    NewConnections =
        case maps:find(OtherNode, Connections) of
            error ->
                ProtoDist:setup(OtherNode, normal, MyNode, Longnames, ?SETUPTIME),
                maps:put(OtherNode, {pending, undefined, DHandle}, Connections);
            {ok, {pending, ConnPid, _}} ->
                maps:put(OtherNode, {pending, ConnPid, DHandle}, Connections);
            {ok, {alive, _ConnPid, _Address}} ->
                Connections
        end,
    {noreply, State#state{connections = NewConnections}}.

%% @hidden
terminate(_Reason, State) ->
    {ok, State}.

split_name(Options) ->
    LongNames = maps:get(name_domain, Options, longnames) =:= longnames,
    NameOption = maps:get(name, Options),
    NameOptionBin = atom_to_binary(NameOption, utf8),
    [NamePart | HostPartL] = binary:split(NameOptionBin, <<"@">>, [global]),
    case HostPartL of
        [HostName] ->
            % Ensure Hostname respects LongNames
            case binary:split(HostName, <<".">>, [global]) of
                [HostName] when not LongNames ->
                    Options#{name => NamePart, node => NameOption};
                [_FirstPart, _SecondPart | _] when LongNames ->
                    Options#{name => NamePart, node => NameOption};
                [HostName] when LongNames ->
                    error(invalid_name, [NameOption, Options]);
                [_FirstPart, _SecondPart | _] when not LongNames ->
                    error(invalid_name, [NameOption, Options])
            end;
        [_HostName, _OtherTrailing] ->
            error(invalid_name, [NameOption, Options]);
        [] ->
            % Use net:gethostname() and truncate if shortnames are used
            {ok, HostName} = net:gethostname(),
            case string:split(HostName, ".") of
                [FirstPart | _] when not LongNames ->
                    NodeNameStr = lists:flatten([binary_to_list(NamePart), "@", FirstPart]),
                    Options#{name => NamePart, node => list_to_atom(NodeNameStr)};
                [HostName] when LongNames ->
                    error(invalid_name, [NameOption, Options]);
                [_FirstPart, _SecondPart] when LongNames ->
                    NodeNameStr = lists:flatten([binary_to_list(NamePart), "@", HostName]),
                    Options#{name => NamePart, node => list_to_atom(NodeNameStr)}
            end
    end.

ticker(Kernel, TickInterval) ->
    receive
    after TickInterval -> ok
    end,
    Kernel ! tick,
    ticker(Kernel, TickInterval).
