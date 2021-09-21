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

-module(port).

-export([call/2, call/3]).

-spec call(pid(), Message::term()) -> term().
call(Pid, Message) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, Message},
    receive
        {Ref, Reply} -> Reply
    end.

-spec call(pid(), Message::term(), TimeoutMs::non_neg_integer()) -> term() | {error, timeout}.
call(Pid, Message, TimeoutMs) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, Message},
    receive
        out_of_memory -> out_of_memory;
        {Ref, Reply} -> Reply
    after
        TimeoutMs ->
            {error, timeout}
    end.
