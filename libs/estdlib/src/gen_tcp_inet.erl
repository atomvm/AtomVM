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

%% @hidden
-module(gen_tcp_inet).

-export([
    connect/3, send/2, recv/2, recv/3, close/1, listen/2, accept/1, accept/2, controlling_process/2
]).

%% inet API
-export([port/1, sockname/1, peername/1]).

-type reason() :: term().

-type option() ::
    {active, boolean()}
    | {buffer, pos_integer()}
    | {timeout, timeout()}
    | list
    | binary
    | {binary, boolean()}.

-type listen_option() :: option().
-type connect_option() :: option().
-type packet() :: string() | binary().

-define(DEFAULT_PARAMS, [{active, true}, {buffer, 512}, {timeout, infinity}]).

%% @hidden
-spec connect(
    Address :: inet:ip_address() | inet:hostname(),
    Port :: inet:port_number(),
    Options :: [connect_option()]
) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: reason()}.
connect(Address, Port, Params0) ->
    Socket = open_port({spawn, "socket"}, []),
    Params = merge(Params0, ?DEFAULT_PARAMS),
    connect(Socket, normalize_address(Address), Port, Params).

%% @hidden
-spec send(Socket :: inet:socket(), Packet :: packet()) -> ok | {error, Reason :: reason()}.
send(Socket, Packet) ->
    case call(Socket, {send, Packet}) of
        {ok, _Len} ->
            ok;
        Error ->
            Error
    end.

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer()) ->
    {ok, packet()} | {error, Reason :: reason()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer(), Timeout :: non_neg_integer()) ->
    {ok, packet()} | {error, Reason :: reason()}.
recv(Socket, Length, Timeout) ->
    call(Socket, {recv, Length, Timeout}).

%% @hidden
-spec listen(Port :: inet:port_number(), Options :: [listen_option()]) ->
    {ok, ListeningSocket :: inet:socket()} | {error, Reason :: reason()}.
listen(Port, Options) ->
    Socket = open_port({spawn, "socket"}, []),
    Params = merge(Options, ?DEFAULT_PARAMS),
    InitParams = [
        {proto, tcp},
        {listen, true},
        {controlling_process, self()},
        {port, Port},
        {backlog, 5}
        | Params
    ],
    case call(Socket, {init, InitParams}) of
        ok ->
            {ok, Socket};
        ErrorReason ->
            %% TODO close port
            ErrorReason
    end.

%% @hidden
-spec accept(ListenSocket :: inet:socket()) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: reason()}.
accept(ListenSocket) ->
    accept(ListenSocket, infinity).

%% @hidden
-spec accept(ListenSocket :: inet:socket(), Timeout :: timeout()) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: reason()}.
accept(ListenSocket, Timeout) ->
    case call(ListenSocket, {accept, Timeout}) of
        {ok, Socket} when is_pid(Socket) ->
            {ok, Socket};
        ErrorReason ->
            %% TODO close port
            ErrorReason
    end.

%% @hidden
-spec close(Socket :: inet:socket()) -> ok.
close(Socket) ->
    call(Socket, {close}),
    ok.

%% @hidden
-spec controlling_process(Socket :: inet:socket(), Pid :: pid()) ->
    ok | {error, Reason :: reason()}.
controlling_process(Socket, Pid) ->
    call(Socket, {controlling_process, Pid}).

%%
%% inet API
%%

%% @hidden
port(Socket) ->
    call(Socket, {get_port}).

%% @hidden
sockname(Socket) ->
    call(Socket, {sockname}).

%% @hidden
peername(Socket) ->
    call(Socket, {peername}).

%% internal operations

%% @private
connect(DriverPid, Address, Port, Params) ->
    InitParams = [
        {proto, tcp},
        {connect, true},
        {controlling_process, self()},
        {address, Address},
        {port, Port}
        | Params
    ],
    case call(DriverPid, {init, InitParams}) of
        ok ->
            {ok, DriverPid};
        ErrorReason ->
            %% TODO close port
            ErrorReason
    end.

%% TODO implement this in lists

%% @private
merge(Config, Defaults) ->
    merge(Config, Defaults, []) ++ Config.

%% @private
merge(_Config, [], Accum) ->
    Accum;
merge(Config, [H | T], Accum) ->
    Key =
        case H of
            {K, _V} -> K;
            K -> K
        end,
    case proplists:get_value(Key, Config) of
        undefined ->
            merge(Config, T, [H | Accum]);
        Value ->
            merge(Config, T, [{Key, Value} | Accum])
    end.

%% @private
normalize_address(localhost) ->
    "127.0.0.1";
normalize_address(loopback) ->
    "127.0.0.1";
normalize_address(Address) when is_list(Address) ->
    Address;
normalize_address({A, B, C, D}) when
    is_integer(A) and is_integer(B) and is_integer(C) and is_integer(D)
->
    integer_to_list(A) ++
        "." ++
        integer_to_list(B) ++
        "." ++
        integer_to_list(C) ++
        "." ++ integer_to_list(D).

%% TODO IPv6

%%
%% Internal operations
%%

%% @private
call(Port, Msg) ->
    case port:call(Port, Msg) of
        {error, noproc} -> {error, closed};
        out_of_memory -> {error, enomem};
        Result -> Result
    end.
