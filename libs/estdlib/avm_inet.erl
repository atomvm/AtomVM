%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
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
-module(avm_inet).

-export([port/1]).
-export([create/2, data/1]).

-type port_number() :: 0..65535.

-record(
    socket, {
        port :: port_number(),
        data :: term()
    }
).

-type socket() :: #socket{}.

-export_type([socket/0, port_number/0]).

%% @hidden
-spec create(Port::port_number(), Data::term()) -> socket().
create(Port, Data) ->
    #socket{port=Port, data=Data}.

%%-----------------------------------------------------------------------------
%% @param   Socket the socket from which to obtain the port number
%% @returns the port number associated with the local socket
%% @doc     Retrieve the actual port number to which the socket is bound.
%%          This function is useful if the port assignment is done by the
%%          operating system.
%% @end
%%-----------------------------------------------------------------------------
-spec port(Socket::socket()) -> port_number().
port(#socket{port=Port}) ->
    Port.

%% @hidden
-spec data(Socket::socket()) -> term().
data(#socket{data=Data}) ->
    Data.
