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
%% This module provides an implementation of the Erlang/OTP gen_udp interface.
%% It is designed to be API-compatible with gen_udp, with exceptions noted
%% below.
%%
%% Caveats:
%% <ul>
%%     <li>Currently no support for IPv6</li>
%%     <li>Currently no support for socket tuning parameters</li>
%%     <li>Receive packet size limited to 128 bytes</li>
%% </ul>
%%
%% <em><b>Note.</b>  Port drivers for this interface are not supported
%% on all AtomVM platforms.</em>
%% @end
%%-----------------------------------------------------------------------------
-module(avm_gen_udp).

-export([open/1, open/2, send/4, recv/2, recv/3]).
-export([get_port_num/1]).

-record(
    socket, {
        pid,
        port
    }
).

-type port_num() :: 0..65535.
-opaque socket() :: #socket{}.
-type proplist() :: [{atom(), any()}].
-type address() :: ipv4_address().
-type ipv4_address() :: {octet(), octet(), octet(), octet()}.
-type octet() :: 0..255.
-type packet() :: string() | binary().
-type reason() :: term().

-export_type([socket/0]).

%%-----------------------------------------------------------------------------
%% @doc     Create a UDP socket.  This function will instatiate a UDP socket
%%          that may be used to
%%          send or receive UDP messages.
%% @equiv   open(Port, [])
%% @end
%%-----------------------------------------------------------------------------
-spec open(port_num()) -> socket().
open(Port) ->
    open(Port, []).

%%-----------------------------------------------------------------------------
%% @param   Port the port number to bind to.  Specify 0 to use an OS-assigned
%%          port number, which can then be retrieved via the get_port_num
%%          function.
%% @param   Params A list of configuration parameters.
%% @returns an opaque reference to the socket instance, used in subsequent
%%          commands.
%% @throws  bad_arg
%% @see     get_port_num/1
%% @doc     Create a UDP socket.  This function will instatiate a UDP socket
%%          that may be used to send or receive UDP messages.
%%          This function will raise an exception with the bad_arg atom if
%%          there is no socket driver supported for the target platform.
%%
%%          <em><b>Note.</b>  The Params argument is currently ignored.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec open(port_num(), proplist()) -> socket().
open(Port, _Params) ->
    Pid = open_port({spawn, "socket"}, []),
    ok = init(Pid, [{proto, udp}]),
    {ok, ActualPort} = bind(Pid, {127, 0, 0, 1}, Port),
    #socket{pid=Pid, port=ActualPort}.

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
-spec send(socket(), address(), port_num(), packet()) -> ok | {error, reason()}.
send(#socket{pid=Pid} = _Socket, Address, Port, Packet) ->
    case call(Pid, {send, Address, Port, Packet}) of
        {ok, _Sent} ->
            ok;
        Else -> Else
    end.

%%-----------------------------------------------------------------------------
%% @equiv   recv(Socket, Length, infinity)
%% @doc     Receive a packet over a UDP socket from a source address/port.
%% @end
%%-----------------------------------------------------------------------------
-spec recv(socket(), non_neg_integer()) ->
    {ok, {address(), port_num(), packet()}} | {error, reason()}.
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
-spec recv(socket(), non_neg_integer(), non_neg_integer()) ->
    {ok, {address(), port_num(), packet()}} | {error, reason()}.
recv(#socket{pid=Pid} = _Socket, Length, Timeout) ->
    call(Pid, {recvfrom, Length, Timeout}).


%%-----------------------------------------------------------------------------
%% @param   Socket the socket from which to obtain the bound port number
%% @returns the port number to which the socket is bound
%% @doc     Retrieve the actual port number to which the socket is bound.
%%          This function is useful if the port assignment is done by the
%%          operating system.
%%
%%          <em><b>Note.</b>  This function is not a part of the Erlang/OTP
%%          gen_udp interface.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec get_port_num(socket()) -> port_num().
get_port_num(#socket{port=Port}) ->
    Port.

%% internal operations

%% @private
init(Pid, Params) ->
    call(Pid, {init, Params}).

%% @private
bind(Pid, Address, Port) ->
    call(Pid, {bind, Address, Port}).

%% @private
call(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {self(),  Ref, Msg},
    receive
        {Ref, Ret} ->
            Ret
    end.
