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

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP gen_udp interface.
%%
%% This module provides an implementation of a subset of the functionality of
%% the Erlang/OTP gen_udp interface.  It is designed to be API-compatible with
%% gen_udp, with exceptions noted below.
%%
%% This interface may be used to send and receive UDP packets, as either
%% binaries or strings.  Active and passive modes are supported for receiving data.
%%
%% Caveats:
%% <ul>
%%     <li>Currently no support for IPv6</li>
%%     <li>Currently limited support for socket tuning parameters</li>
%%     <li>Currently no support for closing sockets</li>
%% </ul>
%%
%% <em><b>Note.</b>  Port drivers for this interface are not supported
%% on all AtomVM platforms.</em>
%% @end
%%-----------------------------------------------------------------------------
-module(gen_udp).

-export([open/1, open/2, send/4, recv/2, recv/3, close/1, controlling_process/2]).

-type packet() :: string() | binary().
-type reason() :: term().

-type option() ::
    {active, boolean()}
    | {buffer, pos_integer()}
    | {timeout, timeout()}
    | list
    | binary
    | {binary, boolean()}
    | {inet_backend, inet | socket}.

-include("inet-priv.hrl").

%%-----------------------------------------------------------------------------
%% @equiv   open(PortNum, [])
%% @doc     Create a UDP socket.  This function will instantiate a UDP socket
%%          that may be used to
%%          send or receive UDP messages.
%% @end
%%-----------------------------------------------------------------------------
-spec open(PortNum :: inet:port_number()) -> {ok, inet:socket()} | {error, Reason :: reason()}.
open(PortNum) ->
    open(PortNum, []).

%%-----------------------------------------------------------------------------
%% @param   Port the port number to bind to.  Specify 0 to use an OS-assigned
%%          port number, which can then be retrieved via the inet:port/1
%%          function.
%% @param   Options A list of configuration parameters.
%% @returns an opaque reference to the socket instance, used in subsequent
%%          commands.
%% @throws  bad_arg
%% @doc     Create a UDP socket.  This function will instantiate a UDP socket
%%          that may be used to send or receive UDP messages.
%%          This function will raise an exception with the bad_arg atom if
%%          there is no socket driver supported for the target platform.
%%
%%          <em><b>Note.</b>  The Params argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec open(PortNum :: inet:port_number(), Options :: [option()]) ->
    {ok, inet:socket()} | {error, Reason :: reason()}.
open(PortNum, Options) ->
    Module = get_inet_backend_module(Options),
    case Module:open(PortNum, Options) of
        {ok, Socket} ->
            {ok, {?GEN_UDP_MONIKER, Socket, Module}};
        Other ->
            Other
    end.

%%-----------------------------------------------------------------------------
%% @param   Socket the socket over which to send a packet
%% @param   Address the target address to which to send the packet
%% @param   Port    the port on target address to which to send the packet
%% @param   Packet  the packet of data to send
%% @returns ok | {error, Reason}
%% @doc     Send a packet over a UDP socket to a target address/port.
%%
%%          <em><b>Note.</b> Currently only ipv4 addresses are supported.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec send(
    Socket :: inet:socket(),
    Address :: inet:ip_address(),
    PortNum :: inet:port_number(),
    Packet :: packet()
) -> ok | {error, reason()}.
send({?GEN_UDP_MONIKER, Socket, Module}, Address, PortNum, Packet) ->
    Module:send(Socket, Address, PortNum, Packet).

%%-----------------------------------------------------------------------------
%% @equiv   recv(Socket, Length, infinity)
%% @doc     Receive a packet over a UDP socket from a source address/port.
%% @end
%%-----------------------------------------------------------------------------
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer()) ->
    {ok, {inet:ip_address(), inet:port_number(), packet()}} | {error, reason()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket over which to receive a packet
%% @param   Length the maximum length to read of the received packet
%% @param   Timeout the amount of time to wait for a packet to arrive
%% @returns {ok, {Address, Port, Packet}} | {error, Reason}
%% @doc     Receive a packet over a UDP socket from a source address/port.
%%          The address and port of the received packet, as well as
%%          the received packet data, are returned from this call.  This
%%          call will block until data is received or a timeout occurs.
%%
%%          <em><b>Note.</b> Currently Length and Timeout parameters are
%%          ignored.</em>
%%
%%          <em><b>Note.</b> Currently the length of the received packet
%%          is limited to 128 bytes.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec recv(Socket :: inet:socket(), Length :: non_neg_integer(), Timeout :: timeout()) ->
    {ok, {inet:ip_address(), inet:port_number(), packet()}} | {error, reason()}.
recv({?GEN_UDP_MONIKER, Socket, Module}, Length, Timeout) ->
    Module:recv(Socket, Length, Timeout).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket to close
%% @returns ok
%% @doc     Close the socket.
%% @end
%%-----------------------------------------------------------------------------
-spec close(inet:socket()) -> ok.
close({?GEN_UDP_MONIKER, Socket, Module}) ->
    Module:close(Socket).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket to which to assign the pid
%% @param   Pid Pid to which to send messages
%% @returns ok | {error, Reason}.
%% @doc     Assign a controlling process to the socket.  The controlling
%% process will receive messages from the socket.
%%
%% This function will return `{error, not_owner}' if the calling process
%% is not the current controlling process.
%%
%% By default, the controlling process is the process associated with
%% the creation of the Socket.
%% @end
%%-----------------------------------------------------------------------------
-spec controlling_process(Socket :: inet:socket(), Pid :: pid()) ->
    ok | {error, Reason :: reason()}.
controlling_process({?GEN_UDP_MONIKER, Socket, Module}, Pid) ->
    Module:controlling_process(Socket, Pid).

%%
%% Internal implementation
%%

%% @private
get_inet_backend_module(Options) ->
    case proplists:get_value(inet_backend, Options) of
        undefined ->
            gen_udp_inet;
        inet ->
            gen_udp_inet;
        socket ->
            gen_udp_socket
    end.
