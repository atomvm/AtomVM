%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Copyright 2017 by Davide Bettio <davide@uninstall.it>                %
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
%% @doc An implementation of the Erlang/OTP lists interface.
%%
%% This module implements a strict susbset of the Erlang/OTP lists
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(avm_lists).

-export([nth/2, member/2, delete/2, reverse/1, keydelete/3, keyfind/3]).

%%-----------------------------------------------------------------------------
%% @param   N the index in the list to get
%% @param   L the list from which to get the value
%% @returns the value in the list at position N.
%% @doc     Get the value in a list at position N.
%%
%%          Returns the value at the specified position in the list.
%%          The bhavior of this function is undefined if N is outside of the
%%          {1..length(L)}.
%% @end
%%-----------------------------------------------------------------------------
-spec nth(N::non_neg_integer(), L::list()) -> term().
nth(1, [H | _T]) ->
    H;
nth(Index, [_H | T]) when Index > 1 ->
    nth(Index - 1, T).

%%-----------------------------------------------------------------------------
%% @param   E the member to search for
%% @param   L the list from which to get the value
%% @returns true if E is a member of L; false, otherwise.
%% @doc     Determine whether a term is a member of a list.
%% @end
%%-----------------------------------------------------------------------------
-spec member(E::term(), L::list()) -> boolean().
member(_, []) ->
    false;
member(E, [E | _]) ->
    true;
member(E, [_ | T]) ->
    member(E, T).

%%-----------------------------------------------------------------------------
%% @param   E the member to delete
%% @param   L the list from which to delete the value
%% @returns the result of removing E from L, if it exists in L; otherwise, L.
%% @doc     Remove E from L
%% @end
%%-----------------------------------------------------------------------------
-spec delete(E::term(), L::list()) -> Result::list().
delete(E, L) ->
    delete(E, L, []).

%% @private
delete(_, [], Accum) ->
    reverse(Accum);
delete(E, [E|T], Accum) ->
    reverse(Accum) ++ T;
delete(E, [H|T], Accum) ->
    delete(E, T, [H|Accum]).

%%-----------------------------------------------------------------------------
%% @param   L the list to reverse
%% @returns the elments of L in reverse order
%% @doc     Reverse the elements of L.
%% @end
%%-----------------------------------------------------------------------------
-spec reverse(list()) -> list().
reverse(L) ->
    %% TODO this should be done in unit time in a BIF
    reverse(L, []).

%% @private
reverse([], Accum) ->
    Accum;
reverse([H|T], Accum) ->
    reverse(T, [H|Accum]).

%%-----------------------------------------------------------------------------
%% @param   K the key to match
%% @param   I the position in the tuple to compare (1..tuple_size)
%% @param   L the list from which to delete the element
%% @returns the result of deleting any element in L who's Ith element matches K
%% @doc     Delete the entry in L whose Ith element matches K.
%% @end
%%-----------------------------------------------------------------------------
-spec keydelete(K::term(), I::pos_integer(), L::list()) -> list().
keydelete(K, I, L) ->
    keydelete(K, I, L, []).

%% @private
keydelete(_K, _I, [], L) ->
    reverse(L);
keydelete(K, I, [H|T], L2) when is_tuple(H) ->
    case I =< tuple_size(H) of
        true ->
            case element(I, H) of
                K ->
                    reverse(L2) ++ T;
                _ ->
                    keydelete(K, I, T, [H|L2])
            end;
        false ->
            keydelete(K, I, T, [H|L2])
    end;
keydelete(K, I, [H|T], L2) ->
    keydelete(K, I, T, [H|L2]).


%%-----------------------------------------------------------------------------
%% @param   K the key to match
%% @param   I the position in the tuple to compare (1..tuple_size)
%% @param   L the list from which to find the element
%% @returns the tuple in L who's Ith element matches K; the atom false, otherwise
%% @doc     Find the entry in L whose Ith element matches K.
%% @end
%%-----------------------------------------------------------------------------
-spec keyfind(K::term(), I::pos_integer(), L::list(tuple())) -> tuple() | false.
keyfind(_K, _I, []) ->
    false;
keyfind(K, I, [H|T]) when is_tuple(H) ->
    case I =< tuple_size(H) of
        true ->
            case element(I, H) of
                K ->
                    H;
                _ ->
                    keyfind(K, I, T)
            end;
        false ->
            keyfind(K, I, T)
    end;
keyfind(K, I, [_H|T]) ->
    keyfind(K, I, T).
