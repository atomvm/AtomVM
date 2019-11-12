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

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP proplists interface.
%%
%% This module implements a strict susbset of the Erlang/OTP proplists
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(avm_proplists).

-export([get_value/2, get_value/3]).

-type property() :: atom() | {term(), term()}.

%%-----------------------------------------------------------------------------
%% @equiv   get_value(Key, List, undefined)
%% @doc     Get a value from a property list.
%% @end
%%-----------------------------------------------------------------------------
-spec get_value(Key::term(), List::list(property())) -> term() | true | undefined.
get_value(Key, List) ->
    get_value(Key, List, undefined).

%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the value
%% @param   List the property list from which to get the value
%% @param   Default the default value to return, if Key is not in the property list.
%% @returns the value in the property list under the key, or Default, if Key is
%%          not in List.
%% @doc     Get a value from a property list.
%%
%%          Returns the value under the specified key, or the specified Default,
%%          if the Key is not in the supplied List.  If the Key corresponds to
%%          an entry in the property list that is just a single atom, this
%%          function returns the atom true.
%% @end
%%-----------------------------------------------------------------------------
-spec get_value(Key::term(), list(property()), Default::term()) -> term().
get_value(_Key, [], Default) ->
    Default;
get_value(Key, [{Key, Value} | _T], _Default) ->
    Value;
get_value(Key, [Key | _T], _Default) when is_atom(Key) ->
    true;
get_value(Key, [_H | T], Default) ->
    get_value(Key, T, Default).
