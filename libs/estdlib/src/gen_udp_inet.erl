%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Fred Dushin <fred@dushin.net>
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
-module(gen_udp_inet).

-export([open/1, open/2, send/4, recv/2, recv/3, close/1, controlling_process/2]).

%% inet API
-export([port/1, sockname/1]).

-type packet() :: string() | binary().
-type reason() :: term().

-type option() ::
    {active, boolean()}
    | {buffer, pos_integer()}
    | {timeout, timeout()}
    | list
    | binary
    | {binary, boolean()}.

-define(DEFAULT_PARAMS, [{active, true}, {buffer, 128}, {timeout, infinity}]).

%% @hidden
-spec open(PortNum :: inet:port_number()) -> {ok, inet:socket()} | {error, Reason :: reason()}.
open(PortNum) ->
    open(PortNum, []).

%% @hidden
-spec open(PortNum :: inet:port_number(), Options :: [option()]) ->
    {ok, inet:socket()} | {error, Reason :: reason()}.
open(PortNum, Options) ->
    DriverPid = open_port({spawn, "socket"}, []),
    Params = merge(Options, ?DEFAULT_PARAMS),
    init(DriverPid, PortNum, Params).

%% @hidden
-spec send(
    Socket :: inet:socket(),
    Address :: inet:ip_address(),
    PortNum :: inet:port_number(),
    Packet :: packet()
) -> ok | {error, reason()}.
send(Socket, Address, PortNum, Packet) ->
    case call(Socket, {sendto, Address, PortNum, Packet}) of
        {ok, _Sent} ->
            ok;
        Else ->
            Else
    end.

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer()) ->
    {ok, {inet:ip_address(), inet:port_number(), packet()}} | {error, reason()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%% @hidden
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer(), Timeout :: timeout()) ->
    {ok, {inet:ip_address(), inet:port_number(), packet()}} | {error, reason()}.
recv(Socket, Length, Timeout) ->
    %% Note.  Currently, timeout is not supported in the native (C) driver
    call(Socket, {recvfrom, Length, Timeout}, Timeout).

%% @hidden
-spec close(inet:socket()) -> ok.
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

%% internal operations

%% @private
init(DriverPid, PortNum, Params) ->
    InitParams = [
        {proto, udp},
        {port, PortNum},
        {controlling_process, self()}
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

%%
%% Internal operations
%%

%% @private
call(Port, Msg) ->
    call(Port, Msg, infinity).

%% @private
call(Port, Msg, Timeout) ->
    case port:call(Port, Msg, Timeout) of
        {error, noproc} -> {error, closed};
        out_of_memory -> {error, enomem};
        Result -> Result
    end.
