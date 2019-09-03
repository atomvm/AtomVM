%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2018 by Fred Dushin <fred@dushin.net>                       %
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
-module(avm_gen_udp).

-export([open/1, open/2, send/4, recv/2, recv/3, close/1]).

-type proplist() :: [{atom(), any()}].
-type address() :: ipv4_address().
-type ipv4_address() :: {octet(), octet(), octet(), octet()}.
-type octet() :: 0..255.
-type packet() :: string() | binary().
-type reason() :: term().

-include("estdlib.hrl").

-define(DEFAULT_PARAMS, [{active, false}, {buffer, 128}, {binary, true}, {address, {127, 0, 0, 1}}, {timeout, infinity}]).

%%-----------------------------------------------------------------------------
%% @doc     Create a UDP socket.  This function will instatiate a UDP socket
%%          that may be used to
%%          send or receive UDP messages.
%% @end
%%-----------------------------------------------------------------------------
-spec open(avm_inet:port_number()) -> {ok, avm_inet:socket()} | {error, Reason::reason()}.
open(PortNum) ->
    open(PortNum, []).

%%-----------------------------------------------------------------------------
%% @param   Port the port number to bind to.  Specify 0 to use an OS-assigned
%%          port number, which can then be retrieved via the inet:port/1
%%          function.
%% @param   Params A list of configuration parameters.
%% @returns an opaque reference to the socket instance, used in subsequent
%%          commands.
%% @throws  bad_arg
%% @doc     Create a UDP socket.  This function will instatiate a UDP socket
%%          that may be used to send or receive UDP messages.
%%          This function will raise an exception with the bad_arg atom if
%%          there is no socket driver supported for the target platform.
%%
%%          <em><b>Note.</b>  The Params argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec open(avm_inet:port_number(), proplist()) -> {ok, avm_inet:socket()} | {error, Reason::reason()}.
open(PortNum, Params0) ->
    DriverPid = open_port({spawn, "socket"}, []),
    Params = merge(Params0, ?DEFAULT_PARAMS),
    init(DriverPid, PortNum, Params).

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
-spec send(avm_inet:socket(), address(), avm_inet:port_number(), packet()) -> ok | {error, reason()}.
send(Socket, Address, PortNum, Packet) ->
    case call(Socket, {send, Address, PortNum, Packet}) of
        {ok, _Sent} ->
            ok;
        Else -> Else
    end.


%%-----------------------------------------------------------------------------
%% @equiv   recv(Socket, Length, infinity)
%% @doc     Receive a packet over a UDP socket from a source address/port.
%% @end
%%-----------------------------------------------------------------------------
-spec recv(avm_inet:socket(), non_neg_integer()) ->
    {ok, {address(), avm_inet:port_number(), packet()}} | {error, reason()}.
recv(Socket, Length) ->
    recv(Socket, Length, infinity).


%%-----------------------------------------------------------------------------
%% @param   Socket the socket over which to receive a packet
%% @param   Length the maximum length to read of the received packet
%% @param   Timeout the amount of time to wait for a packet to arrive
%% @returns {ok, Address, Port, Packet}} | {error, Reason}
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
-spec recv(avm_inet:socket(), non_neg_integer(), non_neg_integer()) ->
    {ok, {address(), avm_inet:port_number(), packet()}} | {error, reason()}.
recv(Socket, Length, Timeout) ->
    internal_recv(Socket, Length, Timeout).


%%-----------------------------------------------------------------------------
%% @param   Socket the socket to close
%% @returns ok, if closing the socket succeeded, or {error, Reason}, if
%%          closing the socket failed for any reason.
%% @doc     Close the socket.
%% @end
%%-----------------------------------------------------------------------------
-spec close(avm_inet:socket()) -> ok | {error, Reason::reason()}.
close(Socket) ->
    call(Socket, {close}).


%% internal operations

%% @private
init(DriverPid, PortNum, Params) ->
    InitParams = [
        {proto, udp}, 
        {port, PortNum}, 
        {controlling_process, self()} | Params
    ],
    case call(DriverPid, {init, InitParams}) of
        ok -> {ok, DriverPid};
        ErrorReason ->
            %% TODO close port
            ErrorReason
    end.

% @private
internal_recv(DriverPid, Length, Timeout) ->
    call(DriverPid, {recvfrom, Length, Timeout}).

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
    merge(Config, Defaults, []).

%% @private
merge(_Config, [], Accum) ->
    Accum;
merge(Config, [H | T], Accum) ->
    Key = case H of
        {K, _V} -> K;
        K -> K
    end,
    case ?PROPLISTS:get_value(Key, Config) of
        undefined ->
            merge(Config, T, [H | Accum]);
        Value ->
            merge(Config, T, [{Key, Value}|Accum])
    end.
