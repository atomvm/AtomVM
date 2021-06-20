%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2021 by Fred Dushin <fred@dushin.net>                       %
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

-module(network_fsm).

-export([
    wait_for_sta/0, wait_for_sta/1, wait_for_sta/2,
    wait_for_ap/0, wait_for_ap/1, wait_for_ap/2
]).
-export([start/1, stop/0]).

wait_for_sta() ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:wait_for_sta().

wait_for_sta(StaConfig) ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:wait_for_sta(StaConfig).

wait_for_sta(StaConfig, Timeout) ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:wait_for_sta(StaConfig, Timeout).

wait_for_ap() ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:wait_for_ap().

wait_for_ap(ApConfig) ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:wait_for_ap(ApConfig).

wait_for_ap(ApConfig, Timeout) ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:wait_for_ap(ApConfig, Timeout).

start(Config) ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:start(Config).

stop() ->
    io:format("WARNING: The network_fsm module is deprecated.  Use the network interface, instead.~n"),
    network:stop().
