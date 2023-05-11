%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
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
    recvfrom/1,
    send/2,
    sendto/3,
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

%%-----------------------------------------------------------------------------
%% @param   Domain the network domain
%% @param   Type the network type
%% @param   Protocol the network protocol
%% @returns `{ok, Socket}' if successful; `{error, Reason}', otherwise.
%% @doc     Create a socket.
%%
%%          Create a socket with a specified domain, type, and protocol.
%%          Use the returned socket for communications.
%%
%%          Currently, the supported values are as follows:
%%          <ul>
%%              <li>`Domain': `inet'</li>
%%              <li>`Type': `stream'</li>
%%              <li>`Protocol': `tcp'</li>
%%          </ul>
%%
%% Example:
%%
%%          `{ok, ListeningSocket} = socket:open(inet, stream, tcp)'
%% @end
%%-----------------------------------------------------------------------------
-spec open(Domain :: socket_domain(), Type :: socket_type(), Protocol :: socket_protocol()) ->
    {ok, socket()} | {error, Reason :: term()}.
open(Domain, Type, Protocol) ->
    open_socket(Domain, Type, Protocol).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Close a socket.
%%
%%          Close a previously opened socket.
%%
%% Example:
%%
%%          `ok = socket:close(Socket)'
%% @end
%%-----------------------------------------------------------------------------
-spec close(Socket :: socket()) -> ok | {error, Reason :: term()}.
close(Socket) when is_pid(Socket) ->
    port:call(Socket, close).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Address the address to which to bind the socket
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Bind a socket to an interface.
%%
%%          Bind a socket to an interface, via a socket address.  Use `any' to
%%          bind to all interfaces.  Use `loopback' to bind to the loopback
%%          address.  To specify a port, use a map containing the network family,
%%          address, and port.
%%
%% Example:
%%
%%          `ok = socket:bind(ListeningSocket, #{family => inet, addr => any, port => 44404})'
%% @end
%%-----------------------------------------------------------------------------
-spec bind(Socket :: socket(), Address :: sockaddr() | any | loopback) ->
    ok | {error, Reason :: term()}.
bind(Socket, Address) when is_pid(Socket) ->
    port:call(Socket, {bind, validate_sock_addr(Address)}).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Set the socket to listen for connections.
%%
%%          Listen for connections.  The socket should be a connection-based
%%          socket and should be bound to an address and port.
%%
%% Example:
%%
%%          `ok = socket:listen(ListeningSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec listen(Socket :: socket()) -> ok | {error, Reason :: term()}.
listen(Socket) when is_pid(Socket) ->
    listen(Socket, ?DEFAULT_BACKLOG).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Backlog the maximum length for the queue of pending connections
%% @returns `ok' if successful; `{error, Reason}', otherwise.
%% @doc     Set the socket to listen for connections.
%%
%%          Listen for connections.  The socket should be a connection-based
%%          socket and should be bound to an address and port.
%%
%%          Use the Backlog to specify the maximum length for the queue of
%%          pending connections
%%
%% Example:
%%
%%          `ok = socket:listen(ListeningSocket, 4)'
%% @end
%%-----------------------------------------------------------------------------
-spec listen(Socket :: socket(), Backlog :: non_neg_integer()) -> ok | {error, Reason :: term()}.
listen(Socket, Backlog) when is_pid(Socket) ->
    port:call(Socket, {listen, validate_non_neg_integer(Backlog)}).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Connection}' if successful; `{error, Reason}', otherwise.
%% @doc     Wait for the socket to accept a connection.
%%
%%          Wait for the socket to accept a connection.  The socket should
%%          be set to listen for connections.
%%
%%          Note that this function will block until a connection is made
%%          from a client.  Typically, users will spawn a call to `accept'
%%          in a separate process.
%%
%% Example:
%%
%%          `{ok, ConnectedSocket} = socket:accept(ListeningSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec accept(Socket :: socket()) -> {ok, Connection :: socket()} | {error, Reason :: term()}.
accept(Socket) when is_pid(Socket) ->
    port:call(Socket, accept).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Address}' if successful; `{error, Reason}', otherwise.
%% @doc     Return the current address for the specified socket.
%%
%% Example:
%%
%%          `{ok, Address} = socket:sockname(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec sockname(Socket :: socket()) -> {ok, Address :: sockaddr()} | {error, Reason :: term()}.
sockname(Socket) when is_pid(Socket) ->
    port:call(Socket, sockname).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Address}' if successful; `{error, Reason}', otherwise.
%% @doc     Return the address of the peer connected to the specified socket.
%%
%% Example:
%%
%%          `{ok, Address} = socket:peername(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
peername(Socket) when is_pid(Socket) ->
    port:call(Socket, peername).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Data}' if successful; `{error, Reason}', otherwise.
%% @doc     Receive data on the specified socket.
%%
%%          Note that this function will block until data is received
%%          on the socket.
%%
%% Example:
%%
%%          `{ok, Data} = socket:recv(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec recv(Socket :: socket()) -> {ok, Data :: binary()} | {error, Reason :: term()}.
recv(Socket) when is_pid(Socket) ->
    port:call(Socket, recv).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, {Address, Data}}' if successful; `{error, Reason}', otherwise.
%% @doc     Receive data on the specified socket, returning the from address.
%%
%%          Note that this function will block until data is received
%%          on the socket.
%%
%% Example:
%%
%%          `{ok, {Address, Data}} = socket:recvfrom(ConnectedSocket)'
%% @end
%%-----------------------------------------------------------------------------
-spec recvfrom(Socket :: socket()) ->
    {ok, {Address :: sockaddr(), Data :: binary()}} | {error, Reason :: term()}.
recvfrom(Socket) when is_pid(Socket) ->
    port:call(Socket, recvfrom).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Data the data to send
%% @returns `{ok, Rest}' if successful; `{error, Reason}', otherwise.
%% @doc     Send data on the specified socket.
%%
%%          Note that this function will block until data is sent
%%          on the socket.  The data may not have been received by the
%%          intended recipient, and the data may not even have been sent
%%          over the network.
%%
%% Example:
%%
%%          `{ok, []} = socket:send(ConnectedSocket, Data)'
%% @end
%%-----------------------------------------------------------------------------
-spec send(Socket :: socket(), Data :: iolist()) ->
    {ok, Rest :: binary()} | {error, Reason :: term()}.
send(Socket, Data) when is_pid(Socket) ->
    port:call(Socket, {send, validate_io_list(Data)}).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Data the data to send
%% @param   Dest the destination to which to send the data
%% @returns `{ok, Rest}' if successful; `{error, Reason}', otherwise.
%% @doc     Send data on the specified socket to the specified destination.
%%
%%          Note that this function will block until data is sent
%%          on the socket.  The data may not have been received by the
%%          intended recipient, and the data may not even have been sent
%%          over the network.
%%
%% Example:
%%
%%          `{ok, []} = socket:sendto(ConnectedSocket, Data, Dest)'
%% @end
%%-----------------------------------------------------------------------------
-spec sendto(Socket :: socket(), Data :: iolist(), Dest :: sockaddr()) ->
    {ok, Rest :: binary()} | {error, Reason :: term()}.
sendto(Socket, Data, Dest) when is_pid(Socket) ->
    port:call(Socket, {sendto, validate_io_list(Data), validate_sock_addr(Dest)}).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @param   Option the option
%% @param   Value the option value
%% @returns `{ok, Address}' if successful; `{error, Reason}', otherwise.
%% @doc     Set a socket option.
%%
%%          Set an option on a socket.
%%
%%          Currently, the following options are supported:
%%          <table>
%%              <tr><td>`{socket, reuseaddr}'</td><td>`boolean()'</td></tr>
%%              <tr><td>`{socket, linger}'</td><td>`#{onoff => boolean(), linger => non_neg_integer()}'</td></tr>
%%          </table>
%%
%% Example:
%%
%%      `ok = socket:setopt(ListeningSocket, {socket, reuseaddr}, true)'
%%      `ok = socket:setopt(ListeningSocket, {socket, linger}, #{onoff => true, linger => 0})'
%% @end
%%-----------------------------------------------------------------------------
setopt(Socket, Option, Value) when is_pid(Socket) ->
    port:call(Socket, {setopt, validate_option(Option), validate_option_value(Option, Value)}).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns `{ok, Connection}' if successful; `{error, Reason}', otherwise.
%% @doc     Wait for the socket to connect to an address.
%%
%%          Wait for the socket to connect to an address.  The socket should
%%          be a connection-based socket.
%%
%%          Note that this function will block until a connection is made
%%          to a server.
%%
%% Example:
%%
%%          `ok = socket:connect(Socket, #{family => inet, addr => loopback, port => 44404})'
%% @end
%%-----------------------------------------------------------------------------
-spec connect(Socket :: socket(), Address :: addr()) -> ok | {error, Reason :: term()}.
connect(Socket, Address) ->
    port:call(Socket, {connect, validate_client_sock_addr(Address)}).

%%
%% Internal functions
%%

%% @private
open_socket(Domain, Type, Protocol) ->
    case open_port({spawn, "otp_socket"}, []) of
        Port when is_port(Port) ->
            case port:call(Port, {init, #{domain => Domain, type => Type, protocol => Protocol}}) of
                ok ->
                    {ok, Port};
                Error ->
                    Error
            end;
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

%% @private
validate_io_list([]) ->
    [];
validate_io_list(Binary) when is_binary(Binary) ->
    Binary;
validate_io_list(Integer) when is_integer(Integer) ->
    Integer;
validate_io_list(L) when is_list(L) ->
    [validate_io_list(E) || E <- L].

%% @private
validate_option({socket, linger} = Option) ->
    Option;
validate_option({socket, reuseaddr} = Option) ->
    Option;
validate_option(_) ->
    error(badarg).

%% @private
validate_option_value({socket, reuseaddr} = _Option, Value) when is_boolean(Value) ->
    Value;
validate_option_value({socket, linger} = _Option, #{onoff := OnOff, linger := Linger} = Value) when
    is_boolean(OnOff) andalso is_integer(Linger) andalso 0 =< Linger
->
    Value;
validate_option_value(_, _) ->
    error(badarg).

%% @private
validate_client_sock_addr(any) ->
    error(badarg);
validate_client_sock_addr(Addr) ->
    validate_sock_addr(Addr).
