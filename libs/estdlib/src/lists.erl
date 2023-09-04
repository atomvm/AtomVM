%
% This file is part of AtomVM.
%
% Copyright 2017-2023 Fred Dushin <fred@dushin.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP lists interface.
%%
%% This module implements a strict susbset of the Erlang/OTP lists
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(lists).

-export([
    map/2,
    nth/2,
    member/2,
    delete/2,
    reverse/1,
    foreach/2,
    keydelete/3,
    keyfind/3,
    keymember/3,
    keyreplace/4,
    foldl/3,
    foldr/3,
    all/2,
    any/2,
    flatten/1,
    search/2,
    filter/2,
    join/2,
    seq/2, seq/3,
    sort/1, sort/2,
    usort/1, usort/2,
    duplicate/2,
    sublist/2
]).

%%-----------------------------------------------------------------------------
%% @param   Fun the function to apply
%% @param   List the list over which to map
%% @returns the result of mapping over L
%% @doc     Map a list of terms, applying Fun(E)
%% @end
%%-----------------------------------------------------------------------------
-spec map(Fun :: fun((Elem :: term()) -> Out :: term()), List :: list()) -> OutList :: term().
map(F, [H | T]) ->
    [F(H) | map(F, T)];
map(F, []) when is_function(F, 1) ->
    [].

%%-----------------------------------------------------------------------------
%% @param   N the index in the list to get
%% @param   L the list from which to get the value
%% @returns the value in the list at position N.
%% @doc     Get the value in a list at position N.
%%
%%          Returns the value at the specified position in the list.
%%          The behavior of this function is undefined if N is outside of the
%%          {1..length(L)}.
%% @end
%%-----------------------------------------------------------------------------
-spec nth(N :: non_neg_integer(), L :: list()) -> term().
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
-spec member(E :: term(), L :: list()) -> boolean().
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
-spec delete(E :: term(), L :: list()) -> Result :: list().
delete(E, L) ->
    delete(E, L, []).

%% @private
delete(_, [], Accum) ->
    reverse(Accum);
delete(E, [E | T], Accum) ->
    reverse(Accum) ++ T;
delete(E, [H | T], Accum) ->
    delete(E, T, [H | Accum]).

%%-----------------------------------------------------------------------------
%% @param   L the list to reverse
%% @returns the elements of L in reverse order
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
reverse([H | T], Accum) ->
    reverse(T, [H | Accum]).

%%-----------------------------------------------------------------------------
%% @param   Fun the predicate to evaluate
%% @param   List the list over which to evaluate elements
%% @returns ok
%% @doc     Applies given fun to each list element
%% @end
%%-----------------------------------------------------------------------------
-spec foreach(Fun :: fun((Elem :: term()) -> term()), List :: list()) -> ok.
foreach(_Fun, []) ->
    ok;
foreach(Fun, [H | T]) ->
    Fun(H),
    foreach(Fun, T).

%%-----------------------------------------------------------------------------
%% @param   K the key to match
%% @param   I the position in the tuple to compare (1..tuple_size)
%% @param   L the list from which to delete the element
%% @returns the result of deleting any element in L who's Ith element matches K
%% @doc     Delete the entry in L whose Ith element matches K.
%% @end
%%-----------------------------------------------------------------------------
-spec keydelete(K :: term(), I :: pos_integer(), L :: list()) -> list().
keydelete(K, I, L) ->
    keydelete(K, I, L, []).

%% @private
keydelete(_K, _I, [], L) ->
    reverse(L);
keydelete(K, I, [H | T], L2) when is_tuple(H) ->
    case I =< tuple_size(H) of
        true ->
            case element(I, H) of
                K ->
                    reverse(L2) ++ T;
                _ ->
                    keydelete(K, I, T, [H | L2])
            end;
        false ->
            keydelete(K, I, T, [H | L2])
    end;
keydelete(K, I, [H | T], L2) ->
    keydelete(K, I, T, [H | L2]).

%%-----------------------------------------------------------------------------
%% @param   K the key to match
%% @param   I the position in the tuple to compare (1..tuple_size)
%% @param   L the list from which to find the element
%% @returns the tuple in L who's Ith element matches K; the atom false, otherwise
%% @doc     Find the entry in L whose Ith element matches K.
%% @end
%%-----------------------------------------------------------------------------
-spec keyfind(K :: term(), I :: pos_integer(), L :: list(tuple())) -> tuple() | false.
keyfind(_K, _I, []) ->
    false;
keyfind(K, I, [H | T]) when is_tuple(H) ->
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
keyfind(K, I, [_H | T]) ->
    keyfind(K, I, T).

%%-----------------------------------------------------------------------------
%% @param   K the key to match
%% @param   I the position in the tuple to compare (1..tuple_size)
%% @param   L the list from which to find the element
%% @returns true if there is a tuple in L who's Ith element matches K; the atom false, otherwise
%% @doc     Returns true if a Ith element matches K.
%% @end
%%-----------------------------------------------------------------------------
-spec keymember(K :: term(), I :: pos_integer(), L :: list(tuple())) -> boolean().
keymember(_K, _I, []) ->
    false;
keymember(K, I, [H | T]) when is_tuple(H) ->
    case I =< tuple_size(H) of
        true ->
            case element(I, H) of
                K ->
                    true;
                _ ->
                    keymember(K, I, T)
            end;
        false ->
            keymember(K, I, T)
    end;
keymember(K, I, [_H | T]) ->
    keymember(K, I, T).

%%-----------------------------------------------------------------------------
%% @param   K           the key to match
%% @param   I           the position in the tuple to compare (1..tuple_size)
%% @param   L           the list from which to find the element
%% @param   NewTuple    tuple containing the new key to replace param `K'
%% @returns result of replacing the first element in L who's Ith element matches K with the contents of NewTuple.
%% @doc     Returns the result of replacing NewTuple for the first element in L with who's Ith element matches K.
%% @end
%%-----------------------------------------------------------------------------
-spec keyreplace(
    K :: term(),
    I :: pos_integer(),
    L :: list(tuple()),
    NewTuple :: {NewKey :: term(), Val :: term()}
) -> boolean().
keyreplace(K, I, L, NewTuple) ->
    keyreplace(K, I, L, L, NewTuple, []).

%% @private
keyreplace(_K, _I, [], OrigL, _NewTuple, _NewList) ->
    OrigL;
keyreplace(K, I, [H | T], L, NewTuple, NewList) when is_tuple(H) andalso is_tuple(NewTuple) ->
    case I =< tuple_size(H) of
        true ->
            case element(I, H) of
                K ->
                    reverse(NewList) ++ [NewTuple | T];
                _ ->
                    keyreplace(K, I, T, L, NewTuple, [H | NewList])
            end;
        false ->
            keyreplace(K, I, T, L, NewTuple, [H | NewList])
    end;
keyreplace(K, I, [H | T], L, NewTuple, NewList) ->
    keyreplace(K, I, T, L, NewTuple, [H | NewList]).

%%-----------------------------------------------------------------------------
%% @param   Fun the function to apply
%% @param   Accum0 the initial accumulator
%% @param   List the list over which to fold
%% @returns the result of folding Fun over L
%% @doc     Fold over a list of terms, from left to right, applying Fun(E, Accum)
%%          to each successive element in List
%% @end
%%-----------------------------------------------------------------------------
-spec foldl(
    Fun :: fun((Elem :: term(), AccIn :: term()) -> AccOut :: term()),
    Acc0 :: term(),
    List :: list()
) -> Acc1 :: term().
foldl(_Fun, Acc0, []) ->
    Acc0;
foldl(Fun, Acc0, [H | T]) ->
    Acc1 = Fun(H, Acc0),
    foldl(Fun, Acc1, T).

%%-----------------------------------------------------------------------------
%% @equiv   foldl(Fun, Acc0, reverse(List))
%% @doc     Fold over a list of terms, from right to left, applying Fun(E, Accum)
%%          to each successive element in List
%% @end
%%-----------------------------------------------------------------------------
-spec foldr(
    Fun :: fun((Elem :: term(), AccIn :: term()) -> AccOut :: term()),
    Acc0 :: term(),
    List :: list()
) -> Acc1 :: term().
foldr(Fun, Acc0, List) ->
    foldl(Fun, Acc0, reverse(List)).

%%-----------------------------------------------------------------------------
%% @param   Fun the predicate to evaluate
%% @param   List the list over which to evaluate elements
%% @returns true if Fun(E) evaluates to true, for all elements in List
%% @doc     Evaluates to true iff Fun(E) =:= true, for all E in List
%% @end
%%-----------------------------------------------------------------------------
-spec all(Fun :: fun((Elem :: term()) -> boolean()), List :: list()) -> boolean().
all(_Fun, []) ->
    true;
all(Fun, [H | T]) ->
    case Fun(H) of
        true ->
            all(Fun, T);
        _ ->
            false
    end.

%%-----------------------------------------------------------------------------
%% @param   Fun the predicate to evaluate
%% @param   List the list over which to evaluate elements
%% @returns true if Fun(E) evaluates to true, for at least one in List
%% @doc     Evaluates to true iff Fun(E) =:= true, for some E in List
%% @end
%%-----------------------------------------------------------------------------
-spec any(Fun :: fun((Elem :: term()) -> boolean()), List :: list()) -> boolean().
any(Fun, L) ->
    not all(fun(E) -> not Fun(E) end, L).

%%-----------------------------------------------------------------------------
%% @param   L the list to flatten
%% @returns flattened list
%% @doc     recursively flattens elements of L into a single list
%% @end
%%-----------------------------------------------------------------------------
-spec flatten(L :: list()) -> list().
flatten(L) when is_list(L) ->
    flatten(L, []).

%% @private
%% pre: Accum is flattened
flatten([], Accum) ->
    Accum;
flatten([H | T], Accum) when is_list(H) ->
    FlattenedT = flatten(T, Accum),
    flatten(H, FlattenedT);
flatten([H | T], Accum) ->
    FlattenedT = flatten(T, Accum),
    [H | FlattenedT].

%% post: return is flattened

%%-----------------------------------------------------------------------------
%% @param   Pred the predicate to apply to elements in List
%% @param   List search
%% @returns the first {value, Val}, if Pred(Val); false, otherwise.
%% @doc     If there is a Value in List such that Pred(Value) returns true,
%%          returns {value, Value} for the first such Value, otherwise returns false.
%% @end
%%-----------------------------------------------------------------------------
-spec search(Pred :: fun((Elem :: term()) -> boolean()), List :: list()) ->
    {value, Value :: term()} | false.
search(_Pred, []) ->
    false;
search(Pred, [H | T]) ->
    case Pred(H) of
        true ->
            {value, H};
        _ ->
            search(Pred, T)
    end.

%%-----------------------------------------------------------------------------
%% @param   Pred the predicate to apply to elements in List
%% @param   List list
%% @returns all values in L for which Pred is true.
%% @doc     Filter a list by a predicate, returning the list of elements
%%          for which the predicate is true.
%% @end
%%-----------------------------------------------------------------------------
-spec filter(Pred :: fun((Elem :: term()) -> boolean()), List :: list()) -> list().
filter(Pred, L) ->
    filter(Pred, L, []).

%% @private
filter(_Pred, [], Accum) ->
    reverse(Accum);
filter(Pred, [H | T], Accum) ->
    case Pred(H) of
        true ->
            filter(Pred, T, [H | Accum]);
        _ ->
            filter(Pred, T, Accum)
    end.

%%-----------------------------------------------------------------------------
%% @param   Sep the separator
%% @param   List list
%% @returns the result of inserting Sep between every element of List.
%% @doc     Inserts Sep between every element of List.
%% @end
%%-----------------------------------------------------------------------------
-spec join(Sep :: any(), List :: list()) -> list().
join(Sep, L) ->
    join(L, Sep, []).

%% @private
join([], _Sep, Accum) ->
    lists:reverse(Accum);
join([E | R], Sep, []) ->
    join(R, Sep, [E]);
join([E | R], Sep, Accum) ->
    join(R, Sep, [E, Sep | Accum]).

%%-----------------------------------------------------------------------------
%% @param   From    from integer
%% @param   To      to Integer
%% @returns list of integers from [From..To]
%% @doc     Returns a sequence of integers in a specified range.
%%
%%          This function is equivalent to `lists:seq(From, To, 1)'.
%% @end
%%-----------------------------------------------------------------------------
-spec seq(From :: integer(), To :: integer()) -> list().
seq(From, To) ->
    seq(From, To, 1).

%%-----------------------------------------------------------------------------
%% @param   From    from integer
%% @param   To      to Integer
%% @param   Incr    increment value
%% @returns list of integers `[From, From+Incr, ..., N]', where `N' is the largest integer `<=' `To' incremented by `Incr'
%% @doc     Returns a sequence of integers in a specified range incremented by a specified value.
%%
%%
%% @end
%%-----------------------------------------------------------------------------
seq(From, To, Incr) when
    (not is_integer(From) orelse not is_integer(To) orelse not is_integer(Incr)) orelse
        (To < (From - Incr) andalso Incr > 0) orelse
        (To > (From - Incr) andalso Incr < 0) orelse
        (Incr =:= 0 andalso From =/= To)
->
    throw(badarg);
seq(To, To, 0) ->
    [To];
seq(From, To, Incr) ->
    seq(From, To, Incr, []).

%% @private
seq(From, To, Incr, Accum) when
    (Incr > 0 andalso From > To) orelse
        (Incr < 0 andalso To > From)
->
    reverse(Accum);
seq(From, To, Incr, Accum) ->
    seq(From + Incr, To, Incr, [From | Accum]).

%%-----------------------------------------------------------------------------
%% @param   List a list
%% @returns Sorted list, ordered by `<'
%% @doc     Returns a sorted list, using `<' operator to determine sort order.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec sort(List :: [T]) -> [T].
sort(List) when is_list(List) ->
    sort(fun lt/2, List).

%%-----------------------------------------------------------------------------
%% @param   Fun sort function
%% @param   List a list
%% @returns Sorted list, ordered by Fun(A, B) : boolean() such that A "less than" B.
%% @doc     Returns a sorted list, using Fun(A, B) to determine sort order.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec sort(Fun :: fun((T, T) -> boolean()), List :: [T]) -> [T].
sort(Fun, List) when is_function(Fun), is_list(List) ->
    quick_sort(Fun, List).

%% Attribution: https://erlang.org/doc/programming_examples/list_comprehensions.html#quick-sort
%% @private
quick_sort(Fun, [Pivot | T]) ->
    quick_sort(Fun, [X || X <- T, Fun(X, Pivot)]) ++
        [Pivot] ++
        quick_sort(Fun, [X || X <- T, not Fun(X, Pivot)]);
quick_sort(_Fun, []) ->
    [].

%% @private
lt(A, B) -> A < B.

%%-----------------------------------------------------------------------------
%% @param   List a list
%% @returns Sorted list with duplicates removed, ordered by `<'
%% @see sort/1
%% @doc     Returns a unique, sorted list, using `<' operator to determine sort order.
%% @end
%%-----------------------------------------------------------------------------
-spec usort(List :: [T]) -> [T].
usort(List) ->
    Sorted = sort(List),
    unique(Sorted).

%%-----------------------------------------------------------------------------
%% @param   Fun sort function
%% @param   List a list
%% @returns Sorted list with duplicates removed, ordered by Fun.
%% @see sort/2
%% @doc     Returns a unique, sorted list.
%% @end
%%-----------------------------------------------------------------------------
-spec usort(Fun :: fun((T, T) -> boolean()), List :: [T]) -> [T].
usort(Fun, List) ->
    Sorted = sort(Fun, List),
    unique(Sorted, Fun).

%% @private
unique(Sorted) ->
    unique(Sorted, fun(X, Y) -> X =< Y end).

%% @private
unique(Sorted, Fun) ->
    unique(Sorted, Fun, []).

%% @private
unique([], _Fun, []) ->
    [];
unique([X], _Fun, Acc) ->
    lists:reverse([X | Acc]);
unique([X, Y | Tail], Fun, Acc) ->
    case Fun(X, Y) andalso Fun(Y, X) of
        true ->
            unique([Y | Tail], Fun, Acc);
        false ->
            unique([Y | Tail], Fun, [X | Acc])
    end.

%%-----------------------------------------------------------------------------
%% @param   Elem the element to duplicate
%% @param   Count the number of times to duplicate the element
%% @returns a list made of Elem duplicate Count times
%% @doc     Duplicate an element
%% @end
%%-----------------------------------------------------------------------------
-spec duplicate(integer(), Elem) -> [Elem].
duplicate(Count, Elem) when is_integer(Count) andalso Count > 0 ->
    duplicate(Count, Elem, []).

duplicate(0, _Elem, Acc) -> Acc;
duplicate(Count, Elem, Acc) -> duplicate(Count - 1, Elem, [Elem | Acc]).

%%-----------------------------------------------------------------------------
%% @param   List list to take the sublist from
%% @param   Len the number of elements to get from List
%% @returns a list made of the first `Len' elements of `List'
%% @doc     Return a sublist made of the first `Len' elements of `List'.
%%          It is not an error for `Len' to be larger than the length of `List'.
%% @end
%%-----------------------------------------------------------------------------
-spec sublist([Elem], integer()) -> [Elem].
sublist(List, Len) when is_integer(Len) andalso Len >= 0 ->
    sublist0(List, Len, []).

%% @private
sublist0(_List, 0, Acc) -> reverse(Acc);
sublist0([], _, Acc) -> reverse(Acc);
sublist0([H | Tail], Len, Acc) -> sublist0(Tail, Len - 1, [H | Acc]).
