%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2018 by Davide Bettio <davide@uninstall.it>                 *
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

%%
%% @doc WARNING: Interfaces around management of  networking are under
%% revision and may change without notice.
%%
-module(network).

-export([setup/1, ifconfig/0]).

setup(Config) ->
    call({setup, Config}).


ifconfig() ->
    call(ifconfig).


%% Internal operations

call(Msg) ->
    Pid = get_pid(),
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, Msg},
    receive
        {Ref, Ret} ->
            Ret
    end.

get_pid() ->
    case erlang:whereis(network) of
        undefined ->
            start();
        Pid -> Pid
    end.

start() ->
    Pid = erlang:open_port({spawn, "network"}, []),
    erlang:register(network, Pid),
    Pid.
