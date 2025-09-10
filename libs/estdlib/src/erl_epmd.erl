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
%% @doc An implementation of the Erlang/OTP erl_epmd interface.
%%
%% This module implements a strict subset of the Erlang/OTP erl_epmd
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(erl_epmd).

% API
-export([
    start_link/0,
    stop/0,
    port_please/2,
    register_node/2,
    names/1
]).

% gen_server
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {socket = undefined}).

-define(EPMD_PORT, 4369).
-define(TIMEOUT, 5000).

-define(NAMES_REQ, 110).
-define(ALIVE2_X_RESP, 118).
-define(PORT2_RESP, 119).
-define(ALIVE2_REQ, 120).
-define(ALIVE2_RESP, 121).
-define(PORT_PLEASE2_REQ, 122).

-define(TCP_INET4_PROTOCOL, 0).
-define(ERLANG_NODE_TYPE, 77).
-define(VERSION, 6).

-record(receive_port2_resp, {
    port_no :: non_neg_integer(),
    highest_version :: non_neg_integer(),
    lowest_version :: non_neg_integer()
}).

-record(alive2_resp, {
    creation :: non_neg_integer()
}).

%% @doc Start EPMD client
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop EPMD client-
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop, infinity).

%% @param Name name of the node to get the port of
%% @param Host host of the node to get the port of
%% @doc Get the port and version of a node on a given host.
%% This function will connect to epmd on the host.
-spec port_please(Name :: iodata(), Host :: inet:hostname() | inet:ip_address()) ->
    {port, inet:port_number(), non_neg_integer()} | noport.
port_please(Name, Host) ->
    case inet:getaddr(Host, inet) of
        {ok, IP} ->
            {ok, Socket} = socket:open(inet, stream, tcp),
            case socket:connect(Socket, #{addr => IP, port => ?EPMD_PORT, family => inet}) of
                ok ->
                    NameBin = iolist_to_binary(Name),
                    Result =
                        case send_request(Socket, <<?PORT_PLEASE2_REQ, NameBin/binary>>) of
                            {ok, #receive_port2_resp{
                                port_no = PortNo,
                                highest_version = HighVersion,
                                lowest_version = LowVersion
                            }} when HighVersion >= ?VERSION andalso LowVersion =< ?VERSION ->
                                {port, PortNo, ?VERSION};
                            {ok, #receive_port2_resp{port_no = PortNo}} ->
                                {port, PortNo, 0};
                            {ok, _Unexpected} ->
                                noport;
                            {error, _} ->
                                noport
                        end,
                    ok = socket:close(Socket),
                    Result;
                {error, _} ->
                    noport
            end;
        {error, _} ->
            noport
    end.

%% @param Host the host to connect to
%% @return a list of names and ports of registered nodes
%% @doc Get the names and ports of all registered nodes
%% This function will connect to epmd on localhost.
-spec names(Host :: inet:hostname() | inet:ip_address()) ->
    {ok, [{string(), inet:port_number()}]} | {error, any()}.
names(Host) ->
    case inet:getaddr(Host, inet) of
        {ok, IP} ->
            {ok, Socket} = socket:open(inet, stream, tcp),
            case socket:connect(Socket, #{addr => IP, port => ?EPMD_PORT, family => inet}) of
                ok ->
                    Result =
                        case socket:send(Socket, <<1:16, ?NAMES_REQ>>) of
                            ok ->
                                case socket:recv(Socket, 4, ?TIMEOUT) of
                                    {ok, <<?EPMD_PORT:32>>} ->
                                        receive_names_loop(Socket, <<>>, []);
                                    {ok, Unexpected} ->
                                        {error, {unexpected, Unexpected}};
                                    {error, _} = ErrRecv ->
                                        ErrRecv
                                end;
                            {error, _} = ErrSend ->
                                ErrSend
                        end,
                    ok = socket:close(Socket),
                    Result;
                {error, _} = ErrConnect ->
                    ErrConnect
            end;
        {error, _} = ErrGetAddr ->
            ErrGetAddr
    end.

receive_names_loop(Socket, AccBuffer, AccL) ->
    case binary:split(AccBuffer, <<"\n">>) of
        [AccBuffer] ->
            case socket:recv(Socket, 0, ?TIMEOUT) of
                {error, closed} when AccBuffer =:= <<>> -> {ok, lists:reverse(AccL)};
                {error, _} = ErrT -> ErrT;
                {ok, Data} -> receive_names_loop(Socket, <<Data/binary, AccBuffer/binary>>, AccL)
            end;
        [<<"name ", RestLine/binary>>, RestBuffer] ->
            case binary:split(RestLine, <<" at port ">>) of
                [NameBin, PortBin] ->
                    try binary_to_integer(PortBin) of
                        Port ->
                            receive_names_loop(Socket, RestBuffer, [
                                {binary_to_list(NameBin), Port} | AccL
                            ])
                    catch
                        error:badarg ->
                            {error, {unexpected, <<"name ", RestLine/binary>>}}
                    end;
                [_] ->
                    {error, {unexpected, <<"name ", RestLine/binary>>}}
            end;
        [UnexpectedLine, _RestBuffer] ->
            {error, {unexpected, UnexpectedLine}}
    end.

%% @param Name name to register
%% @param Port port to register
%% @doc Register to local epmd and get a creation number
-spec register_node(Name :: iodata(), Port :: inet:port_number()) ->
    {ok, non_neg_integer()} | {error, any()}.
register_node(Name, Port) ->
    gen_server:call(?MODULE, {register_node, Name, Port}, infinity).

%% @hidden
init([]) ->
    State = #state{},
    {ok, State}.

%% @hidden
handle_call({register_node, _Name, _Port}, _From, #state{socket = Socket} = State) when
    Socket =/= undefined
->
    {reply, {error, already_registered}, State};
handle_call({register_node, Name, Port}, _From, #state{} = State) ->
    {ok, Socket} = socket:open(inet, stream, tcp),
    case socket:connect(Socket, #{addr => {127, 0, 0, 1}, port => ?EPMD_PORT, family => inet}) of
        ok ->
            NameBin = iolist_to_binary(Name),
            NameLen = byte_size(NameBin),
            Packet =
                <<?ALIVE2_REQ, Port:16, ?ERLANG_NODE_TYPE, ?TCP_INET4_PROTOCOL, ?VERSION:16,
                    ?VERSION:16, NameLen:16, NameBin/binary, 0:16>>,
            case send_request(Socket, Packet) of
                {ok, #alive2_resp{creation = Creation}} ->
                    {reply, {ok, Creation}, State#state{socket = Socket}};
                {error, _} = RequestErr ->
                    socket:close(Socket),
                    {reply, RequestErr, State}
            end;
        {error, _} = ConnectErr ->
            socket:close(Socket),
            {reply, ConnectErr, State}
    end;
handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Message, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, #state{socket = Socket}) ->
    case Socket of
        undefined -> ok;
        _ -> socket:close(Socket)
    end,
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_request(Socket, Request) ->
    RequestSize = byte_size(Request),
    case socket:send(Socket, <<(RequestSize):16, Request/binary>>) of
        ok ->
            case socket:recv(Socket, 1, ?TIMEOUT) of
                {ok, <<?PORT2_RESP>>} ->
                    receive_port2_resp(Socket);
                {ok, <<?ALIVE2_X_RESP>>} ->
                    receive_alive2_x_resp(Socket);
                {ok, <<?ALIVE2_RESP>>} ->
                    receive_alive2_resp(Socket);
                {error, _} = ErrorRecv2 ->
                    ErrorRecv2
            end;
        {error, _} = ErrorSend ->
            ErrorSend
    end.

receive_port2_resp(Socket) ->
    case socket:recv(Socket, 1, ?TIMEOUT) of
        {ok, <<0>>} ->
            case socket:recv(Socket, 10, ?TIMEOUT) of
                {ok,
                    <<PortNo:16, _NodeType, _Protocol, HighestVersion:16, LowestVersion:16,
                        NameLen:16>>} ->
                    case socket:recv(Socket, NameLen + 2, ?TIMEOUT) of
                        {ok, <<_Name:NameLen/binary, ExtraLen:16>>} ->
                            case ExtraLen of
                                0 ->
                                    {ok, #receive_port2_resp{
                                        port_no = PortNo,
                                        highest_version = HighestVersion,
                                        lowest_version = LowestVersion
                                    }};
                                N ->
                                    case socket:recv(Socket, N, ?TIMEOUT) of
                                        {ok, _ExtraData} ->
                                            {ok, #receive_port2_resp{
                                                port_no = PortNo,
                                                highest_version = HighestVersion,
                                                lowest_version = LowestVersion
                                            }};
                                        {error, _} = ErrT1 ->
                                            ErrT1
                                    end
                            end;
                        {error, _} = ErrT2 ->
                            ErrT2
                    end;
                {error, _} = ErrT3 ->
                    ErrT3
            end;
        {ok, <<N>>} ->
            {error, N};
        {error, _} = ErrT4 ->
            ErrT4
    end.

receive_alive2_x_resp(Socket) ->
    case socket:recv(Socket, 5, ?TIMEOUT) of
        {ok, <<0, Creation:32>>} -> {ok, #alive2_resp{creation = Creation}};
        {ok, <<Err, _:32>>} -> {error, Err};
        {error, _} = ErrT -> ErrT
    end.

receive_alive2_resp(Socket) ->
    case socket:recv(Socket, 3, ?TIMEOUT) of
        {ok, <<0, Creation:16>>} -> {ok, #alive2_resp{creation = Creation}};
        {ok, <<Err, _:16>>} -> {error, Err};
        {error, _} = ErrT -> ErrT
    end.
