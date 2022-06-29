%%
%% Copyright 2022 Fred Dushin <fred@dushin.net>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%

-module(socket).

-export([
    open/3,
    close/1,
    bind/2,
    listen/1, listen/2,
    accept/1,
    sockname/1,
    peername/1,
    recv/1,
    send/2,
    setopt/3,
    connect/2
]).

-type socket_domain() :: inet.
-type socket_type() :: stream.
-type socket_protocol() :: tcp.
-type socket() :: term().
-type sockaddr() :: sockaddr_in().
-type sockaddr_in() :: #{
    family := inet,
    port := port_num(),
    addr := addr()
}.
-type addr() :: any | loopback | address_v4().
-type address_v4() :: {octet(), octet(), octet(), octet()}.
-type octet() :: 0..255.
-type port_num() :: 0..65535.

-export_type([socket/0]).

-define(DEFAULT_BACKLOG, 4).

-spec open(Domain :: socket_domain(), Type :: socket_type(), Protocol :: socket_protocol()) ->
    {ok, socket()} | {error, Reason :: term()}.
open(Domain, Type, Protocol) ->
    open_socket(Domain, Type, Protocol).

-spec close(Socket :: socket()) -> ok.
close(Socket) when is_pid(Socket) ->
    port:call(Socket, close).

-spec bind(Socket :: socket(), Address :: sockaddr() | any | localhost) ->
    ok | {error, Reason :: term()}.
bind(Socket, Address) when is_pid(Socket) ->
    port:call(Socket, {bind, validate_sock_addr(Address)}).

-spec listen(Socket :: socket()) -> ok | {error, Reason :: term()}.
listen(Socket) when is_pid(Socket) ->
    listen(Socket, ?DEFAULT_BACKLOG).

-spec listen(Socket :: socket(), Backlog :: non_neg_integer()) -> ok | {error, Reason :: term()}.
listen(Socket, Backlog) when is_pid(Socket) ->
    port:call(Socket, {listen, validate_non_neg_integer(Backlog)}).

-spec accept(Socket :: socket()) -> {ok, Connection :: socket()} | {error, Reason :: term()}.
accept(Socket) when is_pid(Socket) ->
    port:call(Socket, accept).

sockname(Socket) when is_pid(Socket) ->
    port:call(Socket, sockname).

peername(Socket) when is_pid(Socket) ->
    port:call(Socket, peername).

recv(Socket) when is_pid(Socket) ->
    port:call(Socket, recv).

send(Socket, Data) when is_pid(Socket) ->
    port:call(Socket, {send, validate_io_list(Data)}).

setopt(Socket, Option, Value) when is_pid(Socket) ->
    port:call(Socket, {setopt, validate_option(Option), validate_option_value(Option, Value)}).

connect(Socket, Address) ->
    port:call(Socket, {connect, validate_client_sock_addr(Address)}).

%%
%% Internal functions
%%

%% @private
open_socket(Domain, Type, Protocol) ->
    case open_port({spawn, "otp_socket"}, [{domain, Domain}, {type, Type}, {protocol, Protocol}]) of
        Port when is_port(Port) ->
            {ok, Port};
        {error, _Reason} = Error ->
            Error;
        Reason ->
            {error, Reason}
    end.

%%
%% Validation functions
%%

%% @private
validate_sock_addr(loopback) ->
    loopback;
validate_sock_addr(any) ->
    any;
validate_sock_addr(#{addr := Addr, port := Port}) ->
    #{addr => validate_address(Addr), port => validate_port(Port)};
validate_sock_addr(#{addr := Addr}) ->
    #{addr => validate_address(Addr), port => 0};
validate_sock_addr(_) ->
    error(badarg).

%% @private
validate_address(loopback) ->
    loopback;
validate_address(any) ->
    any;
validate_address({A, B, C, D}) ->
    {validate_octet(A), validate_octet(B), validate_octet(C), validate_octet(D)};
validate_address(_) ->
    error(badarg).

%% @private
validate_octet(Octet) when is_integer(Octet) andalso 0 =< Octet andalso Octet =< 255 ->
    Octet;
validate_octet(_) ->
    error(badarg).

%% @private
validate_port(Port) when is_integer(Port) andalso 0 =< Port andalso Port =< 65535 ->
    Port;
validate_port(_) ->
    error(badarg).

%% @private
validate_non_neg_integer(I) when is_integer(I) andalso 0 =< I ->
    I;
validate_non_neg_integer(_) ->
    error(badarg).

validate_io_list([]) ->
    [];
validate_io_list(Binary) when is_binary(Binary) ->
    Binary;
validate_io_list(Integer) when is_integer(Integer) ->
    Integer;
validate_io_list(L) when is_list(L) ->
    [validate_io_list(E) || E <- L].

validate_option({socket, linger} = Option) ->
    Option;
validate_option({socket, reuseaddr} = Option) ->
    Option;
validate_option(_) ->
    error(badarg).

validate_option_value({socket, reuseaddr} = _Option, Value) when is_boolean(Value) ->
    Value;
validate_option_value({socket, linger} = _Option, #{onoff := OnOff, linger := Linger} = Value) when
    is_boolean(OnOff) andalso is_integer(Linger) andalso 0 =< Linger
->
    Value;
validate_option_value(_, _) ->
    error(badarg).

validate_client_sock_addr(any) ->
    error(badarg);
validate_client_sock_addr(Addr) ->
    validate_sock_addr(Addr).
