%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Copyright 2019 by Davide Bettio <davide@uninstall.it>                %
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
%% @doc An implementation of the Erlang/OTP string interface.
%%
%% This module implements a strict susbset of the Erlang/OTP string
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(avm_string).

-export([to_upper/1]).

to_upper(S) when is_list(S) ->
    [upper_char(C) || C <- S];

to_upper(C) when is_integer(C) ->
    upper_char(C).

upper_char(C) when is_integer(C) andalso C >= $a andalso C =< $z ->
    C - 32;

upper_char(C) when is_integer(C) ->
    C.
