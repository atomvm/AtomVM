%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% SPDX-License-Identifier: Apache-2.0
%%

%
% This is shrinked down version of OTP sets module, that supports just the new format
%

-module(sets).
-compile({no_auto_import, [size/1]}).

-export_type([set/0, set/1]).

-export([
    new/0, new/1,
    is_set/1,
    size/1,
    is_empty/1,
    from_list/1,
    from_list/2,
    is_equal/2,
    to_list/1,
    is_element/2,
    add_element/2,
    del_element/2,
    union/1,
    union/2,
    intersection/1,
    intersection/2,
    is_disjoint/2,
    subtract/2,
    is_subset/2,
    fold/3,
    filter/2,
    map/2,
    filtermap/2
]).

-define(VALUE, []).

-type set() :: set(_).

-opaque set(Element) :: #{Element => ?VALUE}.

%%-----------------------------------------------------------------------------
%% @returns Returns a new empty set.
%% @doc     Returns a new empty set using the v2 format (a map).
%% @end
%%-----------------------------------------------------------------------------
-spec new() -> set(none()).
new() -> #{}.

%%-----------------------------------------------------------------------------
%% @param Version must be `{version, 2}'
%% @returns Returns a new empty set.
%% @doc     Returns a new empty set (only v2 format is supported).
%% @end
%%-----------------------------------------------------------------------------
-spec new([{version, 2}]) -> set(none()).
new([{version, 2}] = _Version) ->
    new().

%%-----------------------------------------------------------------------------
%% @param   Set the term that will be checked
%% @returns Return `true' if `Set' is a set of elements, else `false'.
%% @doc     Returns `true' if `Set' appears to be a set of elements, otherwise `false'.
%%
%%          Note that the test is shallow and will return `true' for any term that coincides with
%%          the possible representations of a set.
%% @end
%%-----------------------------------------------------------------------------
-spec is_set(Set) -> boolean() when
    Set :: term().
is_set(#{}) -> true;
is_set(_) -> false.

%%-----------------------------------------------------------------------------
%% @param   Set the set for which size will be returned
%% @returns Return the number of elements in `Set'.
%% @doc     Returns the number of elements in `Set'.
%% @end
%%-----------------------------------------------------------------------------
-spec size(Set) -> non_neg_integer() when
    Set :: set().
size(#{} = S) -> map_size(S).

%%-----------------------------------------------------------------------------
%% @param   Set the set to be checked for emptiness
%% @returns Returns `true' if `Set' is an empty set, otherwise `false'.
%% @doc     Returns `true' if `Set' is an empty set, otherwise `false'.
%% @end
%%-----------------------------------------------------------------------------
-spec is_empty(Set) -> boolean() when
    Set :: set().
is_empty(#{} = S) -> map_size(S) =:= 0.

%%-----------------------------------------------------------------------------
%% @param   List the list of items that is used for building the set
%% @returns Returns a set of the elements in `List'.
%% @doc     Builds a set from the elements in `List'.
%% @end
%%-----------------------------------------------------------------------------
-spec from_list(List) -> Set when
    List :: [Element],
    Set :: set(Element).
from_list(Ls) ->
    maps:from_keys(Ls, ?VALUE).

%%-----------------------------------------------------------------------------
%% @param List the list to be converted to a set
%% @param Version only version 2 is supported
%% @returns Returns a set of the elements in `List' at the given version.
%% @doc     Builds a set from the elements in `List' using the specified version. Only `v2'
%%          format is supported.
%% @end
%%-----------------------------------------------------------------------------
-spec from_list(List, Version :: [{version, 2}]) -> Set when
    List :: [Element],
    Set :: set(Element).
from_list(Ls, [{version, 2}]) ->
    from_list(Ls).

%%-----------------------------------------------------------------------------
%% @param   Set1 first set to be checked for equality
%% @param   Set2 second set to be checked for equality
%% @returns Return `true' if `Set1' and `Set2' contain the same elements, otherwise `false'.
%% @doc     Returns `true' if `Set1' and `Set2' are equal, that is when every element of one
%%          set is also a member of the respective other set, otherwise `false'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec is_equal(Set1, Set2) -> boolean() when
    Set1 :: set(),
    Set2 :: set().
is_equal(S1, S2) ->
    case map_size(S1) =:= map_size(S2) of
        true when S1 =:= S2 ->
            true;
        true ->
            canonicalize_v2(S1) =:= canonicalize_v2(S2);
        false ->
            false
    end.

canonicalize_v2(S) ->
    from_list(to_list(S)).

%%-----------------------------------------------------------------------------
%% @param   Set the set to be converted to a list
%% @returns Return the elements in Set as a list.
%% @doc     Returns the elements of `Set' as a list. The order of the returned elements is
%%          undefined.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec to_list(Set) -> List when
    Set :: set(Element),
    List :: [Element].
to_list(#{} = S) ->
    maps:keys(S).

%%-----------------------------------------------------------------------------
%% @param   Element the element to check
%% @param   Set     the set to check against
%% @returns Returns `true' if `Element' is an element of `Set', otherwise `false'.
%% @doc     Return `true' if `Element' is an element of `Set', else `false'.
%% @end
%%-----------------------------------------------------------------------------
-spec is_element(Element, Set) -> boolean() when
    Set :: set(Element).
is_element(E, #{} = S) ->
    case S of
        #{E := _} -> true;
        _ -> false
    end.

%%-----------------------------------------------------------------------------
%% @param   Element the element to add
%% @param   Set1     the set to add the element to
%% @returns Returns a new set formed from `Set1' with `Element' inserted.
%% @doc     Return `Set1' with `Element' inserted in it.
%% @end
%%-----------------------------------------------------------------------------
-spec add_element(Element, Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).
add_element(E, #{} = S) ->
    S#{E => ?VALUE}.

%%-----------------------------------------------------------------------------
%% @param   Element the element to remove
%% @param   Set1     the set to remove the element from
%% @returns Returns `Set1', but with `Element' removed.
%% @doc     Return `Set1' but with `Element' removed.
%% @end
%%-----------------------------------------------------------------------------
-spec del_element(Element, Set1) -> Set2 when
    Set1 :: set(Element),
    Set2 :: set(Element).
del_element(E, #{} = S) ->
    maps:remove(E, S).

%%-----------------------------------------------------------------------------
%% @param   Set1 the first set
%% @param   Set2 the second set
%% @returns Returns the merged (union) set of `Set1' and `Set2'.
%% @doc     Return the union of `Set1' and `Set2'.
%% @end
%%-----------------------------------------------------------------------------
-spec union(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).
union(#{} = S1, #{} = S2) ->
    maps:merge(S1, S2).

%%-----------------------------------------------------------------------------
%% @param   SetList the list of sets
%% @returns Returns the merged (union) set of the list of sets.
%% @doc     Return the union of the list of sets.
%% @end
%%-----------------------------------------------------------------------------
-spec union(SetList) -> Set when
    SetList :: [set(Element)],
    Set :: set(Element).
union([S1, S2 | Ss]) ->
    union1(union(S1, S2), Ss);
union([S]) ->
    S;
union([]) ->
    new().

-spec union1(set(E), [set(E)]) -> set(E).
union1(S1, [S2 | Ss]) ->
    union1(union(S1, S2), Ss);
union1(S1, []) ->
    S1.

%%-----------------------------------------------------------------------------
%% @param   Set1 the first set
%% @param   Set2 the second set
%% @returns Returns the intersection of `Set1' and `Set2'.
%% @doc     Return the intersection of `Set1' and `Set2'.
%% @end
%%-----------------------------------------------------------------------------
-spec intersection(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).
intersection(#{} = S1, #{} = S2) ->
    case map_size(S1) < map_size(S2) of
        true ->
            Next = maps:next(maps:iterator(S1)),
            intersection_heuristic(Next, [], [], floor(map_size(S1) * 0.75), S1, S2);
        false ->
            Next = maps:next(maps:iterator(S2)),
            intersection_heuristic(Next, [], [], floor(map_size(S2) * 0.75), S2, S1)
    end.

%% If we are keeping more than 75% of the keys, then it is
%% cheaper to delete them. Stop accumulating and start deleting.
intersection_heuristic(Next, _Keep, Delete, 0, Acc, Reference) ->
    intersection_decided(Next, remove_keys(Delete, Acc), Reference);
intersection_heuristic({Key, _Value, Iterator}, Keep, Delete, KeepCount, Acc, Reference) ->
    Next = maps:next(Iterator),
    case Reference of
        #{Key := _} ->
            intersection_heuristic(Next, [Key | Keep], Delete, KeepCount - 1, Acc, Reference);
        _ ->
            intersection_heuristic(Next, Keep, [Key | Delete], KeepCount, Acc, Reference)
    end;
intersection_heuristic(none, Keep, _Delete, _Count, _Acc, _Reference) ->
    maps:from_keys(Keep, ?VALUE).

intersection_decided({Key, _Value, Iterator}, Acc0, Reference) ->
    Acc1 =
        case Reference of
            #{Key := _} -> Acc0;
            #{} -> maps:remove(Key, Acc0)
        end,
    intersection_decided(maps:next(Iterator), Acc1, Reference);
intersection_decided(none, Acc, _Reference) ->
    Acc.

remove_keys([K | Ks], Map) -> remove_keys(Ks, maps:remove(K, Map));
remove_keys([], Map) -> Map.

%%-----------------------------------------------------------------------------
%% @param   SetList the non-empty list of sets
%% @returns Returns the intersection of the non-empty list of sets.
%% @doc     Return the intersection of the list of sets.
%% @end
%%-----------------------------------------------------------------------------
-spec intersection(SetList) -> Set when
    SetList :: [set(Element), ...],
    Set :: set(Element).
intersection([S1, S2 | Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) ->
    S.

-spec intersection1(set(E), [set(E)]) -> set(E).
intersection1(S1, [S2 | Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) ->
    S1.

%%-----------------------------------------------------------------------------
%% @param   Set1 the first set
%% @param   Set2 the second set
%% @returns Returns `true' if `Set1' and `Set2' are disjoint (have no elements in common),
%%          otherwise `false'.
%% @doc     Check whether `Set1' and `Set2' are disjoint.
%% @end
%%-----------------------------------------------------------------------------
-spec is_disjoint(Set1, Set2) -> boolean() when
    Set1 :: set(Element),
    Set2 :: set(Element).
is_disjoint(#{} = S1, #{} = S2) ->
    if
        map_size(S1) < map_size(S2) ->
            is_disjoint_1(S2, maps:iterator(S1));
        true ->
            is_disjoint_1(S1, maps:iterator(S2))
    end.

is_disjoint_1(Set, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            case Set of
                #{K := _} -> false;
                #{} -> is_disjoint_1(Set, NextIter)
            end;
        none ->
            true
    end.

%%-----------------------------------------------------------------------------
%% @param   Set1 the first set
%% @param   Set2 the second set
%% @returns Returns only the elements of `Set1' that are not also elements of `Set2'.
%% @doc     Return all and only the elements of Set1 which are not also in Set2.
%% @end
%%-----------------------------------------------------------------------------
-spec subtract(Set1, Set2) -> Set3 when
    Set1 :: set(Element),
    Set2 :: set(Element),
    Set3 :: set(Element).

subtract(#{} = LHS, #{} = RHS) ->
    LSize = map_size(LHS),
    RSize = map_size(RHS),

    case RSize =< (LSize div 4) of
        true ->
            %% If we're guaranteed to keep more than 75% of the keys, it's
            %% always cheaper to delete them one-by-one from the start.
            Next = maps:next(maps:iterator(RHS)),
            subtract_decided(Next, LHS, RHS);
        false ->
            %% We might delete more than 25% of the keys. Dynamically
            %% transition to deleting elements one-by-one if we can determine
            %% that we'll keep more than 75%.
            KeepThreshold = (LSize * 3) div 4,
            Next = maps:next(maps:iterator(LHS)),
            subtract_heuristic(Next, [], [], KeepThreshold, LHS, RHS)
    end.

subtract_heuristic(Next, _Keep, Delete, 0, Acc, Reference) ->
    %% We've kept more than 75% of the keys, transition to removing them
    %% one-by-one.
    subtract_decided(Next, remove_keys(Delete, Acc), Reference);
subtract_heuristic(
    {Key, _Value, Iterator},
    Keep,
    Delete,
    KeepCount,
    Acc,
    Reference
) ->
    Next = maps:next(Iterator),
    case Reference of
        #{Key := _} ->
            subtract_heuristic(
                Next,
                Keep,
                [Key | Delete],
                KeepCount,
                Acc,
                Reference
            );
        _ ->
            subtract_heuristic(
                Next,
                [Key | Keep],
                Delete,
                KeepCount - 1,
                Acc,
                Reference
            )
    end;
subtract_heuristic(none, Keep, _Delete, _Count, _Acc, _Reference) ->
    maps:from_keys(Keep, ?VALUE).

subtract_decided({Key, _Value, Iterator}, Acc, Reference) ->
    case Reference of
        #{Key := _} ->
            subtract_decided(
                maps:next(Iterator),
                maps:remove(Key, Acc),
                Reference
            );
        _ ->
            subtract_decided(maps:next(Iterator), Acc, Reference)
    end;
subtract_decided(none, Acc, _Reference) ->
    Acc.

%%-----------------------------------------------------------------------------
%% @param   Set1 the first set
%% @param   Set2 the second set
%% @returns Returns `true' when every element of `Set1' is also a member of `Set2',
%%          otherwise `false'.
%% @doc     Return 'true' when every element of Set1 is also a member of Set2, else 'false'.
%% @end
%%-----------------------------------------------------------------------------
-spec is_subset(Set1, Set2) -> boolean() when
    Set1 :: set(Element),
    Set2 :: set(Element).

is_subset(#{} = S1, #{} = S2) ->
    if
        map_size(S1) > map_size(S2) ->
            false;
        true ->
            is_subset_1(S2, maps:iterator(S1))
    end.

is_subset_1(Set, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            case Set of
                #{K := _} -> is_subset_1(Set, NextIter);
                #{} -> false
            end;
        none ->
            true
    end.

%%-----------------------------------------------------------------------------
%% @param   Fun        the function to fold over the elements of the set
%% @param   Accumulator the initial accumulator value
%% @param   Set        the set to fold over
%% @returns Returns the final value of the accumulator after folding the function
%%          over every element in the set.
%% @doc     Fold function Fun over all elements in Set and return Accumulator.
%% @end
%%-----------------------------------------------------------------------------
-spec fold(Function, Acc0, Set) -> Acc1 when
    Function :: fun((Element, AccIn) -> AccOut),
    Set :: set(Element),
    Acc0 :: Acc,
    Acc1 :: Acc,
    AccIn :: Acc,
    AccOut :: Acc.
fold(F, Acc, #{} = D) when is_function(F, 2) ->
    fold_1(F, Acc, maps:iterator(D)).

fold_1(Fun, Acc, Iter) ->
    case maps:next(Iter) of
        {K, _, NextIter} ->
            fold_1(Fun, Fun(K, Acc), NextIter);
        none ->
            Acc
    end.

%%-----------------------------------------------------------------------------
%% @param   Pred the boolean function to filter elements with
%% @param   Set1 the set to filter
%% @returns Returns a set containing elements of `Set1' that satisfy the boolean
%%          function `Fun'. The evaluation order is undefined.
%% @doc     Filter Set with Fun.
%% @end
%%-----------------------------------------------------------------------------
-spec filter(Pred, Set1) -> Set2 when
    Pred :: fun((Element) -> boolean()),
    Set1 :: set(Element),
    Set2 :: set(Element).
filter(F, #{} = D) when is_function(F, 1) ->
    %    %% For this purpose, it is more efficient to use
    %    %% maps:from_keys than a map comprehension.
    %    maps:from_keys([K || K := _ <- D, F(K)], ?VALUE).
    maps:filter(fun(K, _V) -> F(K) end, D).

%%-----------------------------------------------------------------------------
%% @param   Fun the mapping function
%% @param   Set1 the set to map over
%% @returns Returns a set containing elements of `Set1' that are mapped using `Fun'.
%% @doc     Map Set with Fun.
%% @end
%%-----------------------------------------------------------------------------
-spec map(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> Element2),
    Set1 :: set(Element1),
    Set2 :: set(Element2).
map(F, #{} = D) when is_function(F, 1) ->
    %    %% For this purpose, it is more efficient to use
    %    %% maps:from_keys than a map comprehension.
    %    maps:from_keys([F(K) || K := _ <- D], ?VALUE).
    maps:from_keys(lists:map(fun(K) -> F(K) end, maps:keys(D)), ?VALUE).

% Since OTP 27

%%-----------------------------------------------------------------------------
%% @param   Fun the filter and map fun
%% @param   Set1 the set to filter and map over
%% @returns Returns a set containing elements of `Set1' that are filtered and mapped using `Fun'.
%% @doc     Filters and maps elements in `Set1' with function `Fun'.
%% @end
%%-----------------------------------------------------------------------------
-spec filtermap(Fun, Set1) -> Set2 when
    Fun :: fun((Element1) -> boolean() | {true, Element2}),
    Set1 :: set(Element1),
    Set2 :: set(Element1 | Element2).
filtermap(F, #{} = D) when is_function(F, 1) ->
    maps:from_keys(lists:filtermap(F, to_list(D)), ?VALUE).
