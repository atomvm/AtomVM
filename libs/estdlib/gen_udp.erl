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
-module(gen_udp).

-export([open/1, open/2, send/4, recv/2, recv/3]).
-export([get_port/1]).

-record(
    socket, {
        pid,
        port
    }
).

open(Port) ->
    open(Port, []).

open(Port, _Params) ->
    Pid = open_port({spawn, "socket"}, []),
    ok = init(Pid, [{proto, udp}]),
    {ok, ActualPort} = bind(Pid, {127, 0, 0, 1}, Port),
    #socket{pid=Pid, port=ActualPort}.


send(#socket{pid=Pid}, Address, Port, Packet) ->
    case call(Pid, {send, Address, Port, Packet}) of
        {ok, _Sent} ->
            ok;
        Else -> Else
    end.

recv(Socket, Length) ->
    recv(Socket, Length, infinity).
    
recv(#socket{pid=Pid}, Length, Timeout) ->
    call(Pid, {recvfrom, Length, Timeout}).


get_port(#socket{port=Port}) ->
    Port.

%% internal operations

init(Pid, Params) ->
    call(Pid, {init, Params}).

bind(Pid, Address, Port) ->
    call(Pid, {bind, Address, Port}).

call(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {self(),  Ref, Msg},
    receive
        {Ref, Ret} ->
            Ret
    end.
