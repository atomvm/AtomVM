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

%%-----------------------------------------------------------------------------
%% @doc A partial implementation of the Erlang/OTP gen_tcp interface.
%%
%% This module provides an implementation of a subset of the functionality of
%% the Erlang/OTP gen_tcp interface.  It is designed to be API-compatible with
%% gen_tcp, with exceptions noted below.
%%
%% This interface may be used to send and receive TCP packets, as either
%% binaries or strings.  Active and passive modes are supported for receiving data.
%%
%% Caveats:
%% <ul>
%%     <li>Limited support for socket tuning parameters</li>
%%     <li>No support for <b>controlling_process/2</b></li>
%% </ul>
%%
%% <em><b>Note.</b>  Port drivers for this interface are not supported
%% on all AtomVM platforms.</em>
%% @end
%%-----------------------------------------------------------------------------
-module(gen_tcp).

-export([connect/3, send/2, recv/2, recv/3, close/1, listen/2, accept/1, accept/2, controlling_process/2]).

-define(DEFAULT_PARAMS, [{active, true}, {buffer, 512}, {binary, true}, {timeout, infinity}]).

%%-----------------------------------------------------------------------------
%% @param   Address the address to which to connect
%% @param   Port the port to which to connect
%% @param   Options options for controlling the behavior of the socket (see below)
%% @returns {ok, Socket}} | {error, Reason}
%% @doc     Connect to a TCP endpoint on the specified address and port.
%%
%%          If successful, this function will return a Socket which can be used
%%          with the send/2 and revc/2 and recv/3 functions in this module.
%%
%%          The following options are supported:
%%          <ul>
%%              <li><b>active</b> Active mode (default: true)</li>
%%              <li><b>buffer</b> Size of the receive buffer to use in active mode (default: 128)</li>
%%              <li><b>binary</b> If true, receive data as binaries, as opposed to strings (default: true)</li>
%%          </ul>
%%
%%          If the socket is connected in active mode, then the calling process
%%          will receive messages of the form {tcp, Socket, Packet} when
%%          data is received on the socket.  If active mode is set to false, then
%%          applications need to explicitly call one of the recv operations
%%          in order to receive data on the socket.
%% @end
%%-----------------------------------------------------------------------------
-spec connect(inet:address(), inet:port_number(), Options :: inet:opts()) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: term()}.
connect(Address, Port, Params0) ->
    Socket = open_port({spawn, "socket"}, []),
    Params = merge(Params0, ?DEFAULT_PARAMS),
    connect(Socket, normalize_address(Address), Port, Params).

%%-----------------------------------------------------------------------------
%% @param   Socket The Socket obtained via connect/3
%% @param   Packet the data to send
%% @returns ok | {error, Reason}
%% @doc     Send data over the specified socket to a TCP endpoint.
%%
%%          If successful, this function will return the atom ok;
%%          otherwise, an error with a reason.
%% @end
%%-----------------------------------------------------------------------------
-spec send(inet:socket(), inet:packet()) -> ok | {error, Reason :: term()}.
send(Socket, Packet) ->
    case call(Socket, {send, Packet}) of
        {ok, _Len} ->
            ok;
        Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @equiv   recv(Socket, Length, infinity)
%% @doc     Receive a packet over a TCP socket from a source address/port.
%% @end
%%-----------------------------------------------------------------------------
-spec recv(inet:socket(), non_neg_integer()) -> {ok, inet:packet()} | {error, Reason :: term()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket over which to receive a packet
%% @param   Length the maximum length to read of the received packet
%% @param   Timeout the amount of time to wait for a packet to arrive
%% @returns {ok, Address, Port, Packet}} | {error, Reason}
%% @doc     Receive a packet over a TCP socket from a source address/port.
%%
%%          This function is used when the socket is not created in active mode.
%%          The received packet data returned from this call, and should be of
%%          length no greater than the specified length.  This function will return
%%          {error, closed} if the server gracefully terminates the server side
%%          of the connection.
%%
%%          This call will block until data is received or a timeout occurs.
%%
%%          <em><b>Note.</b> Currently, the Timeout parameter is
%%          ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec recv(inet:socket(), non_neg_integer(), non_neg_integer()) ->
    {ok, inet:packet()} | {error, Reason :: term()}.
recv(Socket, Length, Timeout) ->
    call(Socket, {recv, Length, Timeout}).

%%-----------------------------------------------------------------------------
%% @param   Port the port number on which to listen.  Specify 0 to use an OS-assigned
%%          port number, which can then be retrieved via the inet:port/1
%%          function.
%% @param   Options A list of configuration parameters.
%% @returns a listening socket, which is appropriate for use in accept/1
%% @doc     Create a server-side listening socket.
%%
%%          This function is currently unimplemented
%% @end
%%-----------------------------------------------------------------------------
-spec listen(Port :: inet:port_number(), Options :: inet:opts()) ->
    {ok, ListeningSocket :: inet:socket()} | {error, Reason :: term()}.
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

%%-----------------------------------------------------------------------------
%% @param   ListenSocket the listening socket.
%% @returns a connection-based (tcp) socket that can be used for reading and writing
%% @doc     Accept a connection on a listening socket.
%% @end
%%-----------------------------------------------------------------------------
-spec accept(inet:socket()) -> {ok, Socket :: inet:socket()} | {error, Reason :: term()}.
accept(ListenSocket) ->
    accept(ListenSocket, infinity).

%%-----------------------------------------------------------------------------
%% @param   ListenSocket the listening socket.
%% @param   Timeout amount of time in milliseconds to wait for a connection
%% @returns a connection-based (tcp) socket that can be used for reading and writing
%% @doc     Accept a connection on a listening socket.
%% @end
%%-----------------------------------------------------------------------------
-spec accept(inet:socket(), Timeout :: non_neg_integer()) ->
    {ok, Socket :: inet:socket()} | {error, Reason :: term()}.
accept(ListenSocket, Timeout) ->
    case call(ListenSocket, {accept, Timeout}) of
        {ok, Socket} when is_pid(Socket) ->
            {ok, Socket};
        ErrorReason ->
            %% TODO close port
            ErrorReason
    end.

%%-----------------------------------------------------------------------------
%% @param   Socket the socket to close
%% @returns ok.
%% @doc     Close the socket.
%% @end
%%-----------------------------------------------------------------------------
-spec close(inet:socket()) -> ok.
close(Socket) ->
    inet:close(Socket).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket to which to assign the pid
%% @param   Pid Pid to which to send messages
%% @returns ok | {error, Reason}.
%% @doc     Assign a controlling process to the socket.  The controlling
%% process will receive messages from the socket.
%%
%% This function will return `{error, not_owner}' if the calling process
%% is not the current controlling proces.
%%
%% By default, the controlling process is the process associated with
%% the creation of the Socket.
%% @end
%%-----------------------------------------------------------------------------
controlling_process(Socket, Pid) ->
    call(Socket, {controlling_process, Pid}).

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

%% @private
call(DriverPid, Msg) ->
    Ref = erlang:make_ref(),
    DriverPid ! {self(), Ref, Msg},
    receive
        {Ref, Ret} ->
            Ret
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
