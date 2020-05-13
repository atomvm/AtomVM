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
-module(string).

-export([to_upper/1, split/2, split/3, trim/1, trim/2]).

to_upper(S) when is_list(S) ->
    [upper_char(C) || C <- S];

to_upper(C) when is_integer(C) ->
    upper_char(C).

upper_char(C) when is_integer(C) andalso C >= $a andalso C =< $z ->
    C - 32;

upper_char(C) when is_integer(C) ->
    C.

split(String, Pattern) ->
    split(String, Pattern, leading).

split(String, Pattern, Where) ->
    split(String, Pattern, Where, [], []).

%% @private
split([], _Pattern, _Where, Token, Accum) ->
    lists:reverse([lists:reverse(Token)|Accum]);
split(String, Pattern, Where, Token, Accum) ->
    case prefix_match(String, Pattern) of
        {ok, Rest} ->
            case Where of
                leading ->
                    [lists:reverse(Token), Rest];
                all ->
                    split(Rest, Pattern, Where, [], [lists:reverse(Token)|Accum])
            end;
        no ->
            [Char|Rest] = String,
            split(Rest, Pattern, Where, [Char|Token], Accum)
    end.

%% @private
prefix_match(Rest, []) ->
    {ok, Rest};
prefix_match([Char|Rest], [Char|PRest]) ->
    prefix_match(Rest, PRest);
prefix_match(_String, _Pattern) ->
    no.

trim(String) ->
    trim(String, both).

trim(String, leading) ->
    triml(String);
trim(String, trailing) ->
    lists:reverse(triml(lists:reverse(String)));
trim(String, both) ->
    lists:reverse(triml(lists:reverse(triml(String)))).

%% @private
triml([$\s|R]) ->
    triml(R);
triml(R) ->
    R.
