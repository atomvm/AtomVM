%
% This file is part of AtomVM.
%
% Copyright 2017-2023 Fred Dushin <fred@dushin.net>
% split/2 function Copyright Ericsson AB 1996-2023.
% keytake/3 function Copyright Ericsson AB 1996-2024.
% append/1, append/2 functions Copyright Ericsson AB 1996-2025.
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
%% This module implements a strict subset of the Erlang/OTP lists
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(lists).

-export([
    map/2,
    nth/2,
    nthtail/2,
    last/1,
    member/2,
    delete/2,
    reverse/1,
    reverse/2,
    foreach/2,
    keydelete/3,
    keyfind/3,
    keymember/3,
    keyreplace/4,
    keysort/2,
    keystore/4,
    keytake/3,
    foldl/3,
    foldr/3,
    mapfoldl/3,
    all/2,
    any/2,
    flatten/1,
    flatmap/2,
    search/2,
    filter/2,
    filtermap/2,
    join/2,
    seq/2, seq/3,
    sort/1, sort/2,
    merge/2, merge/3,
    split/2,
    usort/1, usort/2,
    ukeysort/2,
    dropwhile/2,
    duplicate/2,
    sublist/2,
    append/1,
    append/2
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
%% @param   N the index to start the sublist from
%% @param   L the list from which to extract a tail
%% @returns a sublist of list starting from position N.
%% @doc     Get the sublist of list L starting after the element N.
%%
%%          The behavior of this function is undefined if N is outside of the
%%          {0..length(L)}.
%% @end
%%-----------------------------------------------------------------------------
-spec nthtail(N :: non_neg_integer(), L :: list()) -> list().
nthtail(0, L) when is_list(L) ->
    L;
nthtail(N, [_H | T]) when is_integer(N) andalso N > 0 ->
    nthtail(N - 1, T).

%%-----------------------------------------------------------------------------
%% @param   L the proper list from which to get the last item
%% @returns the last item of the list.
%% @doc     Get the last item of a list.
%% @end
%%-----------------------------------------------------------------------------
-spec last(L :: nonempty_list(E)) -> E.
last([E]) -> E;
last([_H | T]) -> last(T).

%%-----------------------------------------------------------------------------
%% @param   E the member to search for
%% @param   L the list from which to get the value
%% @returns true if E is a member of L; false, otherwise.
%% @doc     Determine whether a term is a member of a list.
%% @end
%%-----------------------------------------------------------------------------
-spec member(E :: term(), L :: list()) -> boolean().
member(_, []) ->
    erlang:nif_error(undefined).

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
    ?MODULE:reverse(Accum);
delete(E, [E | T], Accum) ->
    ?MODULE:reverse(Accum) ++ T;
delete(E, [H | T], Accum) ->
    delete(E, T, [H | Accum]).

%%-----------------------------------------------------------------------------
%% @param   L the list to reverse
%% @returns the elements of L in reverse order
%% @equiv lists:reverse(L, [])
%% @doc Erlang/OTP implementation of this function actually handles few simple
%% cases and calls `lists:reverse/2' for the more generic case. Consequently,
%% calling `lists:reverse/1' without a list or with an improper list of two
%% elements will fail with a function clause exception on Erlang/OTP and with a
%% badarg exception with this implementation.
%% @end
%%-----------------------------------------------------------------------------
-spec reverse(L :: list()) -> list().
reverse(_L) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   L the list to reverse
%% @param   T the tail to append to the reversed list
%% @returns the elements of L in reverse order followed by T
%% @doc     Reverse the elements of L, followed by T.
%% If T is not a list or not a proper list, it is appended anyway and the result
%% will be an improper list.
%%
%% If L is not a proper list, the function fails with badarg.
%%
%% Following Erlang/OTP tradition, `lists:reverse/1,2' is a nif. It computes
%% the length and then allocates memory for the list at once (2 * n terms).
%%
%% While this is much faster with AtomVM as allocations are expensive with
%% default heap growth strategy, it can consume more memory until the list
%% passed is garbage collected, as opposed to a recursive implementation where
%% the process garbage collect part of the input list during the reversal.
%%
%% Consequently, tail-recursive implementations calling `lists:reverse/2'
%% can be as expensive or more expensive in memory than list comprehensions or
%% non-tail recursive versions depending on the number of terms saved on the
%% stack between calls.
%%
%% For example, a non-tail recursive join/2 implementation requires two terms
%% on stack for each iteration, so when it returns it will use
%% `n * 3' (stack) + `n * 4' (result list)
%% a tail recursive version will use, on last iteration:
%% `n * 4' (reversed list) + n * 4' (result list)
%% @end
%%-----------------------------------------------------------------------------
-spec reverse
    (L :: nonempty_list(E), T :: list(E)) -> nonempty_list(E);
    (L :: nonempty_list(), T :: any()) -> maybe_improper_list();
    (L :: [], T) -> T when T :: any().
reverse(_L, _T) ->
    erlang:nif_error(undefined).

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
    ?MODULE:reverse(L);
keydelete(K, I, [H | T], L2) when is_tuple(H) ->
    case I =< tuple_size(H) of
        true ->
            case element(I, H) of
                K ->
                    ?MODULE:reverse(L2, T);
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
keyfind(_K, _I, _L) ->
    erlang:nif_error(undefined).

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
    erlang:nif_error(undefined).

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
    Key :: term(),
    N :: pos_integer(),
    TupleList :: [tuple()],
    NewTuple :: tuple()
) -> [tuple()].
keyreplace(Key, N, TupleList, NewTuple) when is_tuple(NewTuple) ->
    case keyreplace(Key, N, TupleList, TupleList, NewTuple, []) of
        {false, _Reversed} -> TupleList;
        {value, Updated} -> Updated
    end.

%% @private
keyreplace(_Key, _N, [], _OrigL, _NewTuple, Acc) ->
    {false, Acc};
keyreplace(Key, N, [H | Tail], _OrigL, NewTuple, Acc) when element(N, H) =:= Key ->
    {value, ?MODULE:reverse(Acc, [NewTuple | Tail])};
keyreplace(Key, N, [H | Tail], OrigL, NewTuple, Acc) ->
    keyreplace(Key, N, Tail, OrigL, NewTuple, [H | Acc]).

%%-----------------------------------------------------------------------------
%% @param   N           the position in the tuple to compare (1..tuple_size)
%% @param   L           the list to sort
%% @returns The list L sorted by Nth element
%% @doc     Sort a list of tuples by Nth element.
%% @end
%%-----------------------------------------------------------------------------
-spec keysort(N :: pos_integer(), L :: [tuple()]) -> [tuple()].
keysort(N, TupleList) ->
    sort(fun(E1, E2) -> element(N, E1) < element(N, E2) end, TupleList).

%%-----------------------------------------------------------------------------
%% @param   Key         the key to match
%% @param   N           the position in the tuple to compare (1..tuple_size)
%% @param   TupleList   the list of tuples from which to find the element
%% @param   NewTuple    the tuple to add to the list
%% @returns An updated TupleList where the first occurrence of `Key' has been
%%          replaced with `NewTuple'.
%% @doc     Searches the list of tuples `TupleList' for a tuple whose `N'th
%%          element compares equal to `Key', replaces it with `NewTuple' if
%%          found. If not found, append `NewTuple' to `TupleList'.
%% @end
%%-----------------------------------------------------------------------------
-spec keystore(
    Key :: term(),
    N :: pos_integer(),
    TupleList :: [tuple()],
    NewTuple :: tuple()
) -> [tuple()].
keystore(Key, N, TupleList, NewTuple) when is_tuple(NewTuple) ->
    case keyreplace(Key, N, TupleList, TupleList, NewTuple, []) of
        {false, Reversed} -> ?MODULE:reverse(Reversed, [NewTuple]);
        {value, Updated} -> Updated
    end.

%%-----------------------------------------------------------------------------
%% @param   Key         the key to match
%% @param   N           the position in the tuple to compare (1..tuple_size)
%% @param   TupleList1  the list of tuples from which to find the element
%% @returns `{value, Tuple, TupleList2}' if such a tuple is found, otherwise `false'.
%%          `TupleList2' is a copy of `TupleList1' where the first
%%          occurrence of `Tuple' has been removed.
%% @doc     Searches the list of tuples `TupleList1' for a tuple whose `N'th element
%%          compares equal to `Key'.
%% @end
%%-----------------------------------------------------------------------------
-spec keytake(Key, N, TupleList1) -> {value, Tuple, TupleList2} | false when
    Key :: term(),
    N :: pos_integer(),
    TupleList1 :: [tuple()],
    TupleList2 :: [tuple()],
    Tuple :: tuple().
keytake(Key, N, L) when is_integer(N), N > 0 ->
    keytake(Key, N, L, []).

keytake(Key, N, [H | T], L) when element(N, H) == Key ->
    {value, H, lists:reverse(L, T)};
keytake(Key, N, [H | T], L) ->
    keytake(Key, N, T, [H | L]);
keytake(_K, _N, [], _L) ->
    false.

%%-----------------------------------------------------------------------------
%% @param   Fun the function to apply
%% @param   Acc0 the initial accumulator
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
%% @param   Fun the function to apply
%% @param   Acc0 the initial accumulator
%% @param   List the list over which to fold
%% @returns the result of mapping and folding Fun over L
%% @doc     Combine `map/2' and `foldl/3' in one pass.
%% @end
%%-----------------------------------------------------------------------------
-spec mapfoldl(fun((A, Acc) -> {B, Acc}), Acc, [A]) -> {[B], Acc}.
mapfoldl(Fun, Acc0, List1) ->
    mapfoldl0(Fun, {[], Acc0}, List1).

mapfoldl0(_Fun, {List1, Acc0}, []) ->
    {?MODULE:reverse(List1), Acc0};
mapfoldl0(Fun, {List1, Acc0}, [H | T]) ->
    {B, Acc1} = Fun(H, Acc0),
    mapfoldl0(Fun, {[B | List1], Acc1}, T).

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
    foldl(Fun, Acc0, ?MODULE:reverse(List)).

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
%% @doc     Flattens elements of L into a single list
%% @end
%%-----------------------------------------------------------------------------
-spec flatten(L :: list()) -> list().
flatten(_L) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   F the function to apply to elements of L
%% @param   L the list to map
%% @returns List of mapped elements, appended
%% @doc     Map elements of list L by applying F, and append the results. F should
%% return a list.
%% @end
%%-----------------------------------------------------------------------------
-spec flatmap(F :: fun((Elem :: term()) -> [term()]), List :: [term()]) -> [term()].
flatmap(F, L) when is_list(L) ->
    flatmap_1(F, L).

flatmap_1(_F, []) ->
    [];
flatmap_1(F, [H | T]) ->
    F(H) ++ flatmap_1(F, T).

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
filter(Pred, L) when is_function(Pred, 1) ->
    [X || X <- L, Pred(X)].

% Taken from `otp/blob/master/lib/stdlib/src/lists.erl`

%%-----------------------------------------------------------------------------
%% @param   Fun the filter/map fun
%% @param   List1 the list where given fun will be applied
%% @returns Returns the result of application of given fun over given list items
%% @doc     Calls `Fun(Elem)' on successive elements `Elem' of `List1' in order to update or
%%          remove elements from `List1'.
%%
%%          `Fun/1' must return either a Boolean or a tuple `{true, Value}'. The function
%%          returns the list of elements for which `Fun' returns a new value, where a value
%%          of `true' is synonymous with `{true, Elem}'.
%%
%%          Example:
%%          `1> lists:filtermap(fun(X) -> case X rem 2 of 0 -> {true, X div 2}; _ -> false end end, [1,2,3,4,5]).'
%%          `[1,2]'
%%
%% @end
%%-----------------------------------------------------------------------------
-spec filtermap(Fun, List1) -> List2 when
    Fun :: fun((Elem) -> boolean() | {'true', Value}),
    List1 :: [Elem],
    List2 :: [Elem | Value],
    Elem :: term(),
    Value :: term().

filtermap(F, List) when is_function(F, 1) ->
    filtermap_1(F, List).

filtermap_1(F, [Hd | Tail]) ->
    case F(Hd) of
        true ->
            [Hd | filtermap_1(F, Tail)];
        {true, Val} ->
            [Val | filtermap_1(F, Tail)];
        false ->
            filtermap_1(F, Tail)
    end;
filtermap_1(_F, []) ->
    [].

%%-----------------------------------------------------------------------------
%% @param   Sep the separator
%% @param   List list
%% @returns the result of inserting Sep between every element of List.
%% @doc     Inserts Sep between every element of List.
%% @end
%%-----------------------------------------------------------------------------
-spec join(Sep :: any(), List :: list()) -> list().
join(_Sep, []) ->
    [];
join(Sep, [H | Tail]) ->
    [H | join_1(Sep, Tail)].

%% @private
join_1(Sep, [H | Tail]) ->
    [Sep, H | join_1(Sep, Tail)];
join_1(_Sep, []) ->
    [].

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
seq(From, To) when is_integer(From) andalso is_integer(To) andalso From =< To ->
    seq_r(From, To, 1, []);
seq(From, To) when is_integer(From) andalso is_integer(To) andalso From =:= To + 1 ->
    [].

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
-spec seq(From :: integer(), To :: integer(), Incr :: integer()) -> list().
seq(From, To, Incr) when
    (not is_integer(From) orelse not is_integer(To) orelse not is_integer(Incr)) orelse
        (To < (From - Incr) andalso Incr > 0) orelse
        (To > (From - Incr) andalso Incr < 0) orelse
        (Incr =:= 0 andalso From =/= To)
->
    error(badarg);
seq(To, To, 0) ->
    [To];
seq(From, To, Incr) when From =:= To + Incr ->
    [];
seq(From, To, Incr) ->
    Last = From + ((To - From) div Incr) * Incr,
    seq_r(From, Last, Incr, []).

%% @private
seq_r(From, From, _Incr, Acc) -> [From | Acc];
seq_r(From, To, Incr, Acc) -> seq_r(From, To - Incr, Incr, [To | Acc]).

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
sort(Fun, List) when is_function(Fun, 2), is_list(List) ->
    merge_sort(Fun, List).

merge_sort(_Fun, []) ->
    [];
merge_sort(_Fun, [_] = L) ->
    L;
merge_sort(Fun, List) ->
    {H1, H2} = merge_sort_split(List, List, []),
    merge(Fun, merge_sort(Fun, H1), merge_sort(Fun, H2), []).

merge_sort_split([], Half1, Half2) ->
    {lists:reverse(Half2), Half1};
merge_sort_split([_], Half1, Half2) ->
    {lists:reverse(Half2), Half1};
merge_sort_split([_, _ | T], [H | Half1T], Half2) ->
    merge_sort_split(T, Half1T, [H | Half2]).

%%-----------------------------------------------------------------------------
%% @param   N elements non negative Integer
%% @param   List1 list to split
%% @returns Tuple with the two lists
%% @doc     Splits List1 into List2 and List3. List2 contains the first N elements
%%          and List3 the remaining elements (the Nth tail).
%% @end
%%-----------------------------------------------------------------------------
%% Attribution: https://github.com/erlang/otp/blob/5c8a9cbd125f3db5f5d13ff5ba2a12c076912425/lib/stdlib/src/lists.erl#L1801
-spec split(N, List1) -> {List2, List3} when
    N :: non_neg_integer(),
    List1 :: [T],
    List2 :: [T],
    List3 :: [T],
    T :: term().

split(N, List) when is_integer(N), N >= 0, is_list(List) ->
    case split(N, List, []) of
        {_, _} = Result ->
            Result;
        Fault when is_atom(Fault) ->
            erlang:error(Fault, [N, List])
    end;
split(N, List) ->
    erlang:error(badarg, [N, List]).

split(0, L, R) ->
    {lists:reverse(R, []), L};
split(N, [H | T], R) ->
    split(N - 1, T, [H | R]);
split(_, [], _) ->
    badarg.

%%-----------------------------------------------------------------------------
%% @param   List1 first list to merge, previously sorted
%% @param   List2 second list to merge, previously sorted
%% @returns Merged list of List1 and List2
%% @doc     Returns a list formed by merging List1 and List2, following natural
%%          order. If elements compare equal, element from List1 is picked first
%% @end
%%-----------------------------------------------------------------------------
merge(List1, List2) ->
    merge(fun lt/2, List1, List2, []).

%%-----------------------------------------------------------------------------
%% @param   Fun ordering function
%% @param   List1 first list to merge, previously sorted
%% @param   List2 second list to merge, previously sorted
%% @returns Merged list of List1 and List2
%% @doc     Returns a list formed by merging List1 and List2, following Fun
%%          order. If elements compare equal, element from List1 is picked first
%% @end
%%-----------------------------------------------------------------------------
merge(Fun, List1, List2) ->
    merge(Fun, List1, List2, []).

merge(_Fun, [], Right, Acc) ->
    lists:reverse(Acc, Right);
merge(_Fun, Left, [], Acc) ->
    lists:reverse(Acc, Left);
merge(Fun, [A | As], [B | Bs], Acc) ->
    % keep sort stable, if B < A, put it first, otherwise keep A first
    case Fun(B, A) of
        true -> merge(Fun, [A | As], Bs, [B | Acc]);
        false -> merge(Fun, As, [B | Bs], [A | Acc])
    end.

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
unique([], _Fun) ->
    [];
unique([X], _Fun) ->
    [X];
unique([X, Y | Tail], Fun) ->
    case Fun(X, Y) andalso Fun(Y, X) of
        true ->
            unique([Y | Tail], Fun);
        false ->
            [X | unique([Y | Tail], Fun)]
    end.

%%-----------------------------------------------------------------------------
%% @param   N           the position in the tuple to compare (1..tuple_size)
%% @param   L           the list to sort
%% @returns The list L sorted by Nth element with duplicates removed
%% @doc     Sort a list of tuples by Nth element and remove duplicates.
%%          Two tuples compare equal if their Nth elements compare equal.
%% @end
%%-----------------------------------------------------------------------------
-spec ukeysort(N :: pos_integer(), L :: [tuple()]) -> [tuple()].
ukeysort(N, TupleList) ->
    Sorted = keysort(N, TupleList),
    unique_key(Sorted, N).

%% @private
unique_key([], _N) ->
    [];
unique_key([X], _N) ->
    [X];
unique_key([X, Y | Tail], N) when element(N, X) =:= element(N, Y) ->
    unique_key([X | Tail], N);
unique_key([X, Y | Tail], N) ->
    [X | unique_key([Y | Tail], N)].

%%-----------------------------------------------------------------------------
%% @param   Pred the predicate to check against elements in List1
%% @param   List1
%% @returns List List1 tail at the point predicate returned false for a element
%% @doc     Drops elements Elem from List1 while Pred(Elem) returns true and returns
%%          the remaining list. The Pred function must return a boolean.
%% @end
%%-----------------------------------------------------------------------------
% Attribution: https://github.com/erlang/otp/blob/05737d130706c7189a8e6750d9c2252d2cc7987e/lib/stdlib/src/lists.erl#L2403
-spec dropwhile(Pred, List1) -> List2 when
    Pred :: fun((Elem :: T) -> boolean()),
    List1 :: [T],
    List2 :: [T],
    T :: term().

dropwhile(Pred, List) when is_function(Pred, 1) ->
    dropwhile_1(Pred, List).

dropwhile_1(Pred, [Hd | Tail] = Rest) ->
    case Pred(Hd) of
        true -> dropwhile_1(Pred, Tail);
        false -> Rest
    end;
dropwhile_1(_Pred, []) ->
    [].

%%-----------------------------------------------------------------------------
%% @param   Elem the element to duplicate
%% @param   Count the number of times to duplicate the element
%% @returns a list made of Elem duplicate Count times
%% @doc     Duplicate an element
%% @end
%%-----------------------------------------------------------------------------
-spec duplicate(integer(), Elem) -> [Elem].
duplicate(Count, Elem) when is_integer(Count) andalso Count >= 0 ->
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
    sublist0(List, Len).

%% @private
sublist0([], _Len) -> [];
sublist0(_, 0) -> [];
sublist0([H | Tail], Len) -> [H | sublist0(Tail, Len - 1)].

%%-----------------------------------------------------------------------------
%% @param   ListOfLists a list of lists to make the general list from
%% @returns a list made of the sublists of `ListOfLists'.
%% @doc     Returns a list in which all the sublists of `ListOfLists' have been appended.
%% @end
%%-----------------------------------------------------------------------------
%% Attribution: https://github.com/erlang/otp/blob/34f92c2a9cbb37ef22aecf6b95613d210509015a/lib/stdlib/src/lists.erl#L222
-spec append(ListOfLists) -> List1 when
    ListOfLists :: [List],
    List :: [T],
    List1 :: [T],
    T :: term().

append([E]) -> E;
append([H | T]) -> H ++ append(T);
append([]) -> [].

%%-----------------------------------------------------------------------------
%% @param   List1 a list
%% @param   List2 a list
%% @returns a list made of the elements of `List1' and `List2'.
%% @doc     Returns a new list `List3', which is made from the elements of `List1' followed by the elements of `List2'.
%% @end
%%-----------------------------------------------------------------------------
%% Attribution: https://github.com/erlang/otp/blob/35037ba900e15bd98e778e567079b426531d0085/lib/stdlib/src/lists.erl#L202
-spec append(List1, List2) -> List3 when
    List1 :: [T],
    List2 :: [T],
    List3 :: [T],
    T :: term().

append(L1, L2) -> L1 ++ L2.
