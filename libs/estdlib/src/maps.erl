%
% This file is part of AtomVM.
%
% Copyright 2021 Fred Dushin <fred@dushin.net>
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
%% @doc A <em>naive</em> implementation of the Erlang/OTP `maps' interface.
%%
%% The `maps' module provides several convenience operations for interfacing
%% with the Erlang map type, which associates (unique) keys with values.
%%
%% Note that the ordering of entries in a map is implementation-defined.  While
%% many operations in this module present entries in lexical order, users should
%% in general make no assumptions about the ordering of entries in a map.
%%
%% This module implements a subset of the Erlang/OTP `maps' interface.
%% Some OTP functions are not implemented, and the approach favors
%% correctness and readability over speed and performance.
%% @end
%%-----------------------------------------------------------------------------
-module(maps).

-export([
    get/2, get/3,
    is_key/2,
    is_iterator_valid/1,
    put/3,
    iterator/1,
    iterator/2,
    next/1,
    new/0,
    keys/1,
    values/1,
    to_list/1,
    from_list/1,
    size/1,
    find/2,
    filter/2,
    filtermap/2,
    fold/3,
    foreach/2,
    from_keys/2,
    groups_from_list/2,
    groups_from_list/3,
    intersect/2,
    intersect_with/3,
    map/2,
    merge/2,
    merge_with/3,
    remove/2,
    take/2,
    update/3,
    update_with/3,
    update_with/4,
    with/2,
    without/2
]).

-export_type([
    iterator/2,
    iterator/0,
    iterator_order/1,
    iterator_order/0
]).

-opaque iterator(Key, Value) ::
    {Key, Value, iterator()}
    | none
    | nonempty_improper_list(non_neg_integer(), #{Key => Value})
    | nonempty_improper_list(list(Key), #{Key => Value}).
-type iterator() :: iterator(Key :: any(), Value :: any()).
-type iterator_order(Key) :: undefined | ordered | reversed | fun((Key, Key) -> boolean()).
-type iterator_order() :: iterator_order(Key :: term()).
-type map_or_iterator(Key, Value) :: #{Key => Value} | iterator(Key, Value).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to get
%% @param   Map     the map from which to get the value
%% @returns the value in `Map' associated with `Key', if it exists.
%% @doc     Get the value in `Map' associated with `Key', if it exists.
%%
%% This function raises a `{badkey, Key}' error if 'Key' does not occur in
%% `Map' or a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec get(Key, Map :: #{Key => Value}) -> Value.
get(Key, Map) ->
    erlang:map_get(Key, Map).

%%-----------------------------------------------------------------------------
%% @param   Key     the key
%% @param   Map     the map
%% @param   Default default value
%% @returns the value in `Map' associated with `Key', or `Default', if
%%          the key is not associated with a value in `Map'.
%% @doc     Get the value in `Map' associated with `Key', or `Default', if
%%          the key is not associated with a value in `Map'.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec get(Key, Map :: #{Key => Value}, Default :: Value) -> Value.
get(Key, Map, Default) ->
    try
        ?MODULE:get(Key, Map)
    catch
        error:{badkey, _} ->
            Default
    end.

%%-----------------------------------------------------------------------------
%% @param   Key     the key
%% @param   Map     the map
%% @returns `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%% @doc     Return `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec is_key(Key, Map :: #{Key => _Value}) -> boolean().
is_key(Key, Map) ->
    erlang:is_map_key(Key, Map).

%%-----------------------------------------------------------------------------
%% @param   Iterator the iterator to validate
%% @returns `true' if the iterator is valid, `false' otherwise
%% @doc Check if an iterator is valid.
%%
%% This function checks if an iterator can still be used with `maps:next/1'.
%% An iterator becomes invalid if it has been exhausted or if the underlying
%% map has been modified.
%%
%% This is an internal function, primarily used by other functions in this module.
%% @end
%%-----------------------------------------------------------------------------
-spec is_iterator_valid(Iterator :: iterator()) -> boolean().
is_iterator_valid(Iterator) ->
    try is_iterator_valid_1(Iterator)
    catch
        error:badarg -> false
    end.

%%-----------------------------------------------------------------------------
%% @param   Map     the map
%% @returns A copy of `Map' containing the `{Key, Value}' association.
%% @doc     Return the map containing the `{Key, Value}' association.
%%
%% If `Key' occurs in `Map' then it will be over-written.  Otherwise, the
%% returned map will contain the new association.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec put(Key, Value, Map :: #{Key => Value}) -> #{Key => Value}.
put(Key, Value, Map) when is_map(Map) ->
    Map#{Key => Value};
put(_Key, _Value, Map) when not is_map(Map) ->
    error({badmap, Map}).

%% @equiv iterator(Map, undefined)
-spec iterator(Map :: #{Key => Value}) -> iterator(Key, Value).
iterator(Map) ->
    iterator(Map, undefined).

%%-----------------------------------------------------------------------------
%% @param   Map     the map
%% @param   Order   the iterator order, or undefined for default (unspecified)
%% order.
%% @returns an iterator structure that can be used to iterate over associations
%% in a map.
%% @see next/1
%% @doc Return an iterator structure that can be used to iterate over associations
%% in a map.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec iterator(Map :: #{Key => Value}, Order :: iterator_order()) -> iterator(Key, Value).
iterator(Map, undefined) when is_map(Map) ->
    [0 | Map];
iterator(Map, Order) when is_map(Map) ->
    Keys = iterate_keys(maps:next(maps:iterator(Map)), Order, []),
    [Keys | Map];
iterator(Map, _Order) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Iterator a map iterator
%% @returns the key and value, along with the next iterator in the map, or the
%%          atom `none' if there are no more items over which to iterate.
%% @doc Returns the next key and value in the map, along with
%% a new iterator that can be used to iterate over the remainder of the map.
%%
%% This function raises a `badarg' error if the supplied iterator is not
%% of the expected type.  Only use iterators that are returned from functions
%% in this module.
%% @end
%%-----------------------------------------------------------------------------
-spec next(Iterator :: iterator(Key, Value)) ->
    {Key, Value, iterator(Key, Value)} | none.
next([_Pos | _Map] = _Iterator) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns a new map
%% @doc Return a new (empty) map.
%% @end
%%-----------------------------------------------------------------------------
-spec new() -> map().
new() ->
    #{}.

%%-----------------------------------------------------------------------------
%% @param   Map     the map
%% @returns the list of keys that occur in this map.
%% @doc Returns the list of keys that occur in this map.
%%
%% No guarantees are provided about the order of keys returned from this function.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec keys(Map :: #{Key => _Value}) -> [Key].
keys(Map) when is_map(Map) ->
    iterate_keys(maps:next(maps:iterator(Map)), undefined, []);
keys(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Map     the map
%% @returns the list of values that occur in this map.
%% @doc Returns the list of values that occur in this map.
%%
%% No guarantees are provided about the order of values returned from this function.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec values(Map :: #{_Key => Value}) -> [Value].
values(Map) when is_map(Map) ->
    iterate_values(maps:next(maps:iterator(Map)), []);
values(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   MapOrIterator     the map or iterator
%% @returns a list of `[{Key, Value}]' tuples
%% @doc Return the list of entries, expressed as `{Key, Value}' pairs, in the supplied map.
%%
%% If provided with a map, no guarantees are provided about the order of
%% entries returned from this function. Order can be controlled with `iterator/2'
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map and not
%% an iterator.
%% @end
%%-----------------------------------------------------------------------------
-spec to_list(Map :: #{Key => Value}) -> [{Key, Value}].
to_list(Map) when is_map(Map) ->
    to_list(maps:iterator(Map));
to_list(Iterator) when is_list(Iterator) andalso is_map(tl(Iterator)) ->
    iterate_entries(maps:next(Iterator), []);
to_list(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   List a list of `[{Key, Value}]' pairs
%% @returns the map containing the entries from the list of supplied key-value pairs.
%% @doc This function constructs a map from the supplied list of key-value pairs.
%%
%% If the input list contains duplicate keys, the returned map will contain the
%% right-most entry.
%%
%% This function will raise a `badarg' error if the input is not a proper
%% list or contains an element that is not a key-value pair.
%% @end
%%-----------------------------------------------------------------------------
-spec from_list(List :: [{Key, Value}]) -> #{Key => Value}.
from_list(List) when is_list(List) ->
    iterate_from_list(List, ?MODULE:new());
from_list(_List) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Map the map
%% @returns the size of the map
%% @doc Returns the size of (i.e., the number of entries in) the map
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec size(Map :: map()) -> non_neg_integer().
size(Map) when is_map(Map) ->
    erlang:map_size(Map);
size(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to find
%% @param   Map     the map in which to search
%% @returns `{ok, Value}' if `Key' is in `Map'; `error', otherwise.
%% @doc Returns `{ok, Value}' if `Key' is in `Map'; `error', otherwise.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec find(Key, Map :: #{Key => Value}) -> {ok, Value} | error.
find(Key, Map) ->
    try
        {ok, ?MODULE:get(Key, Map)}
    catch
        _:{badkey, _} ->
            error
    end.

%%-----------------------------------------------------------------------------
%% @param   Pred    a function used to filter entries from the map
%% @param   MapOrIterator the map or map iterator to filter
%% @returns a map containing all elements in `MapOrIterator' that satisfy `Pred'
%% @doc Return a map who's entries are filtered by the supplied predicate.
%%
%% This function returns a new map containing all elements from the input
%% `MapOrIterator' that satisfy the input `Pred'.
%%
%% The supplied predicate is a function from key-value inputs to a boolean value.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map or map
%% iterator, and a `badarg' error if the input predicate is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec filter(
    Pred :: fun((Key, Value) -> boolean()),
    MapOrIterator :: map_or_iterator(Key, Value)
) -> #{Key => Value}.
filter(Pred, Map) when is_function(Pred, 2) andalso is_map(Map) ->
    iterate_filter(Pred, maps:next(maps:iterator(Map)), ?MODULE:new());
filter(Pred, [Pos | Map] = Iterator) when
    is_function(Pred, 2) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_filter(Pred, maps:next(Iterator), ?MODULE:new());
filter(_Pred, Map) when not is_map(Map) ->
    error({badmap, Map});
filter(_Pred, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Fun     a function that maps and filters entries from the map
%% @param   MapOrIterator the map or map iterator to filter and map
%% @returns a map containing all elements in `MapOrIterator' that satisfy `Fun'
%% @doc Return a map whose entries are filtered and mapped by the supplied function.
%%
%% This function returns a new map containing all elements from the input
%% `MapOrIterator' that satisfy the input `Fun'.
%%
%% The supplied function is a function from key-value inputs to either `true'
%% (keep the entry), `false' (drop the entry), or `{true, NewValue}' (keep the
%% entry with a new value).
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map or map
%% iterator, and a `badarg' error if the input function is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec filtermap(
    Fun :: fun((Key, Value) -> boolean() | {true, NewValue}),
    MapOrIterator :: map_or_iterator(Key, Value)
) -> #{Key => Value | NewValue}.
filtermap(Fun, Map) when is_function(Fun, 2) andalso is_map(Map) ->
    maps:from_list(iterate_filtermap(Fun, maps:next(maps:iterator(Map)), []));
filtermap(Fun, [Pos | Map] = Iterator) when
    is_function(Fun, 2) andalso is_integer(Pos) andalso is_map(Map)
->
    maps:from_list(iterate_filtermap(Fun, maps:next(Iterator), []));
filtermap(_Fun, Map) when not is_map(Map) ->
    error({badmap, Map});
filtermap(_Fun, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Fun     function over which to fold values
%% @param   Init    the initial value of the fold accumulator
%% @param   MapOrIterator the map or map iterator over which to fold
%% @returns the result of folding over all elements of the supplied map.
%% @doc Fold over the entries in a map.
%%
%% This function takes a function used to fold over all entries in a map
%% and an initial accumulator value to use as the value supplied to the
%% first entry in the map.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator,
%% and a `badarg' error if the input function is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec fold(
    Fun :: fun((Key, Value, Accum) -> Accum),
    Accum,
    MapOrIterator :: map_or_iterator(Key, Value)
) -> Accum.
fold(Fun, Init, Map) when is_function(Fun, 3) andalso is_map(Map) ->
    iterate_fold(Fun, maps:next(maps:iterator(Map)), Init);
fold(Fun, Init, [Pos | Map] = Iterator) when
    is_function(Fun, 3) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_fold(Fun, maps:next(Iterator), Init);
fold(_Fun, _Init, Map) when not is_map(Map) ->
    error({badmap, Map});
fold(_Fun, _Init, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Fun     function to call with every key-value pair
%% @param   MapOrIterator the map or map iterator over which to iterate
%% @returns `ok'
%% @doc Iterate over the entries in a map.
%%
%% This function takes a function used to iterate over all entries in a map.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator,
%% and a `badarg' error if the input function is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec foreach(
    Fun :: fun((Key, Value) -> any()),
    MapOrIterator :: map_or_iterator(Key, Value)
) -> ok.
foreach(Fun, Map) when is_function(Fun, 2) andalso is_map(Map) ->
    iterate_foreach(Fun, maps:next(maps:iterator(Map)));
foreach(Fun, [Pos | Map] = Iterator) when
    is_function(Fun, 2) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_foreach(Fun, maps:next(Iterator));
foreach(_Fun, Map) when not is_map(Map) ->
    error({badmap, Map});
foreach(_Fun, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   List    the list of keys of the map that will be created
%% @param   Value   the value that will be used as value for all map items
%% @returns a map having all provided keys having provided value as value
%% @doc Creates a map with specified keys intialized to given value
%% @end
%%-----------------------------------------------------------------------------
-spec from_keys(list(), term()) -> map().
from_keys(List, _Value) when is_list(List) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Fun     a function that returns the key for each element
%% @param   List    the list to group
%% @returns a map where keys are the results of applying `Fun' to elements
%%          and values are lists of elements that produced that key
%% @doc Group elements of a list by a key function.
%%
%% This function groups elements of `List' into a map. The key for each element
%% is computed by applying `Fun' to the element. All elements with the same key
%% are collected into a list, preserving the order from the original list.
%%
%% This function raises a `badarg' error if `Fun' is not a function of arity 1
%% or if `List' is not a proper list.
%% @end
%%-----------------------------------------------------------------------------
-spec groups_from_list(Fun :: fun((Elem) -> Key), List :: [Elem]) -> #{Key => [Elem]}.
groups_from_list(Fun, List) when is_function(Fun, 1) ->
    groups_from_list(Fun, fun(X) -> X end, List);
groups_from_list(_Fun, _List) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   KeyFun  a function that returns the key for each element
%% @param   ValueFun a function that returns the value for each element
%% @param   List    the list to group
%% @returns a map where keys are the results of applying `KeyFun' to elements
%%          and values are lists of results of applying `ValueFun' to elements
%%          that produced that key
%% @doc Group elements of a list by a key function, with value transformation.
%%
%% This function groups elements of `List' into a map. The key for each element
%% is computed by applying `KeyFun' to the element, and the value is computed
%% by applying `ValueFun' to the element. All elements with the same key
%% are collected into a list, preserving the order from the original list.
%%
%% This function raises a `badarg' error if `KeyFun' or `ValueFun' are not
%% functions of arity 1 or if `List' is not a proper list.
%% @end
%%-----------------------------------------------------------------------------
-spec groups_from_list(
    KeyFun :: fun((Elem) -> Key),
    ValueFun :: fun((Elem) -> Value),
    List :: [Elem]
) -> #{Key => [Value]}.
groups_from_list(KeyFun, ValueFun, List) when
    is_function(KeyFun, 1) andalso is_function(ValueFun, 1)
->
    try lists:reverse(List) of
        RevList ->
            groups_from_list_1(KeyFun, ValueFun, RevList, #{})
    catch
        error:_ ->
            error(badarg)
    end;
groups_from_list(_KeyFun, _ValueFun, _List) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Map1  a map
%% @param   Map2  a map
%% @returns a map containing the intersection of `Map1' and `Map2'
%% @doc Return the intersection of two maps.
%%
%% This function returns a new map containing only those keys that exist in
%% both `Map1' and `Map2'. The values are taken from `Map2'.
%%
%% This function raises a `badmap' error if either `Map1' or `Map2' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec intersect(Map1 :: #{Key => Value}, Map2 :: #{Key => Value}) -> #{Key => Value}.
intersect(Map1, Map2) when is_map(Map1) andalso is_map(Map2) ->
    case map_size(Map1) =< map_size(Map2) of
        true ->
            intersect_with_small_map_first(fun(_K, _V1, V2) -> V2 end, Map1, Map2);
        false ->
            intersect_with_small_map_first(fun(_K, V1, _V2) -> V1 end, Map2, Map1)
    end;
intersect(Map1, _Map2) when not is_map(Map1) ->
    error({badmap, Map1});
intersect(_Map1, Map2) when not is_map(Map2) ->
    error({badmap, Map2}).

%%-----------------------------------------------------------------------------
%% @param   Combiner  a function to combine values from Map1 and Map2
%% @param   Map1  a map
%% @param   Map2  a map
%% @returns a map containing the intersection of `Map1' and `Map2' with combined values
%% @doc Return the intersection of two maps with combined values.
%%
%% This function returns a new map containing only those keys that exist in
%% both `Map1' and `Map2'. For each such key, the value is computed by calling
%% `Combiner(Key, Value1, Value2)' where `Value1' is from `Map1' and `Value2'
%% is from `Map2'.
%%
%% This function raises a `badmap' error if either `Map1' or `Map2' is not a map,
%% and a `badarg' error if `Combiner' is not a function of arity 3.
%% @end
%%-----------------------------------------------------------------------------
-spec intersect_with(
    Combiner :: fun((Key, Value, Value) -> Value),
    Map1 :: #{Key => Value},
    Map2 :: #{Key => Value}
) -> #{Key => Value}.
intersect_with(Combiner, Map1, Map2) when
    is_map(Map1) andalso is_map(Map2) andalso is_function(Combiner, 3)
->
    case map_size(Map1) =< map_size(Map2) of
        true ->
            intersect_with_small_map_first(Combiner, Map1, Map2);
        false ->
            RCombiner = fun(K, V1, V2) -> Combiner(K, V2, V1) end,
            intersect_with_small_map_first(RCombiner, Map2, Map1)
    end;
intersect_with(_Combiner, Map1, _Map2) when not is_map(Map1) ->
    error({badmap, Map1});
intersect_with(_Combiner, _Map1, Map2) when not is_map(Map2) ->
    error({badmap, Map2});
intersect_with(_Combiner, _Map1, _Map2) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Fun     the function to apply to every entry in the map
%% @param   Map     the map to which to apply the map function
%% @returns the result of applying `Fun' to every entry in `Map'
%% @doc Returns the result of applying a function to every element of a map.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator,
%% and a `badarg' error if the input function is not a function.
%% @end
%%-----------------------------------------------------------------------------
-spec map(Fun :: fun((Key, Value) -> MappedValue), Map :: map_or_iterator(Key, Value)) ->
    #{Key => MappedValue}.
map(Fun, Map) when is_function(Fun, 2) andalso is_map(Map) ->
    iterate_map(Fun, maps:next(maps:iterator(Map)), ?MODULE:new());
map(Fun, [Pos | Map] = Iterator) when
    is_function(Fun, 2) andalso is_integer(Pos) andalso is_map(Map)
->
    iterate_map(Fun, maps:next(Iterator), ?MODULE:new());
map(_Fun, Map) when not is_map(Map) ->
    error({badmap, Map});
map(_Fun, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Map1  a map
%% @param   Map2  a map
%% @returns the result of merging entries from `Map1' and `Map2'.
%% @doc Merge two maps to yield a new map.
%%
%% If `Map1' and `Map2' contain the same key, then the value from `Map2' will be used.
%%
%% This function raises a `badmap' error if neither `Map1' nor `Map2' is a map.
%% @end
%%-----------------------------------------------------------------------------
-spec merge(Map1 :: #{Key => Value}, Map2 :: #{Key => Value}) -> #{Key => Value}.
merge(Map1, Map2) when is_map(Map1) andalso is_map(Map2) ->
    iterate_merge(maps:next(maps:iterator(Map2)), Map1);
merge(Map1, _Map2) when not is_map(Map1) ->
    error({badmap, Map1});
merge(_Map1, Map2) when not is_map(Map2) ->
    error({badmap, Map2}).

%%-----------------------------------------------------------------------------
%% @param   Combiner  a function to merge values from Map1 and Map2 if a key exists in both maps
%% @param   Map1  a map
%% @param   Map2  a map
%% @returns the result of merging entries from `Map1' and `Map2'.
%% @doc Merge two maps to yield a new map.
%%
%% If `Map1' and `Map2' contain the same key, then the value from `Combiner(Key, Value1, Value2)' will be used.
%%
%% This function raises a `badmap' error if neither `Map1' nor `Map2' is a map.
%% @end
%%-----------------------------------------------------------------------------
-spec merge_with(
    Combiner :: fun((Key, Value, Value) -> Value), Map1 :: #{Key => Value}, Map2 :: #{Key => Value}
) -> #{Key => Value}.
merge_with(Combiner, Map1, Map2) when is_map(Map1) andalso is_map(Map2) ->
    iterate_merge_with(Combiner, maps:next(maps:iterator(Map1)), Map2);
merge_with(_Combiner, Map1, _Map2) when not is_map(Map1) ->
    error({badmap, Map1});
merge_with(_Combiner, _Map1, Map2) when not is_map(Map2) ->
    error({badmap, Map2}).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to remove
%% @param   MapOrIterator     the map or map iterator from which to remove the key
%% @returns a new map without `Key' as an entry.
%% @doc Remove an entry from a map using a key.
%%
%% If `Key' does not occur in `Map', then the returned Map has the same
%% entries as the input map or map iterator.
%%
%% Note.  This function extends the functionality of the OTP `remove/2' function,
%% since the OTP interface only takes a map as input.
%%
%% This function raises a `badmap' error if `Map' is not a map or map iterator.
%% @end
%%-----------------------------------------------------------------------------
-spec remove(Key, MapOrIterator :: map_or_iterator(Key, Value)) -> #{Key => Value}.
remove(Key, Map) when is_map(Map) ->
    case ?MODULE:is_key(Key, Map) of
        true ->
            iterate_remove(Key, maps:next(maps:iterator(Map)), ?MODULE:new());
        _ ->
            Map
    end;
remove(Key, [Pos | Map] = Iterator) when is_integer(Pos) andalso is_map(Map) ->
    iterate_remove(Key, maps:next(Iterator), ?MODULE:new());
remove(_Key, Map) when not is_map(Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to update
%% @param   Value   the value to update
%% @param   Map     the map to update
%% @returns a new map, with `Key' updated with `Value'
%% @doc Returns a new map with an updated key-value association.
%%
%% This function raises a `badmap' error if `Map' is not a map and
%% `{badkey, Key}` if key doesn't exist
%% @end
%%-----------------------------------------------------------------------------
-spec update(Key, Value, Map :: #{Key => Value}) -> #{Key => Value}.
update(Key, Value, Map) ->
    _ = ?MODULE:get(Key, Map),
    Map#{Key => Value}.

%%-----------------------------------------------------------------------------
%% @param   Key     the key to take
%% @param   Map     the map from which to take the key
%% @returns `{Value, Map2}' if `Key' exists in `Map', where `Value' is the
%%          value associated with `Key' and `Map2' is the map without `Key'.
%%          Returns `error' if `Key' is not present in `Map'.
%% @doc Removes the `Key' from `Map' and returns the associated value together
%% with the updated map.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%% @end
%%-----------------------------------------------------------------------------
-spec take(Key, Map1 :: #{Key => Value, _ => _}) -> {Value, Map2 :: #{_ => _}} | error.
take(Key, Map) when is_map(Map) ->
    case Map of
        #{Key := Value} -> {Value, maps:remove(Key, Map)};
        _ -> error
    end;
take(_Key, Map) ->
    error({badmap, Map}).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to update
%% @param   Fun     the function to apply to the existing value
%% @param   Map     the map to update
%% @returns a new map with `Key' updated by applying `Fun' to its existing value.
%% @doc Updates the value in `Map' for `Key' by calling `Fun' with the old value.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map,
%% a `{badkey, Key}' error if `Key' is not present in `Map', and a `badarg'
%% error if `Fun' is not a function of arity 1.
%% @end
%%-----------------------------------------------------------------------------
-spec update_with(Key, Fun :: fun((Value1) -> Value2), Map1 :: #{Key := Value1, _ => _}) ->
    #{Key := Value2, _ => _}.
update_with(Key, Fun, Map) when is_function(Fun, 1) andalso is_map(Map) ->
    case Map of
        #{Key := Value} -> Map#{Key := Fun(Value)};
        #{} -> error({badkey, Key})
    end;
update_with(_Key, _Fun, Map) when not is_map(Map) ->
    error({badmap, Map});
update_with(_Key, _Fun, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to update
%% @param   Fun     the function to apply to the existing value
%% @param   Init    the default value to insert if `Key' is not present
%% @param   Map     the map to update
%% @returns a new map with `Key' updated by `Fun', or inserted with `Init'
%%          if `Key' was not present.
%% @doc Updates the value in `Map' for `Key' by calling `Fun' on the old value,
%% or inserts `Init' if `Key' was not previously present.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map and a
%% `badarg' error if `Fun' is not a function of arity 1.
%% @end
%%-----------------------------------------------------------------------------
-spec update_with(
    Key,
    Fun :: fun((Value1) -> Value2),
    Init,
    Map1 :: #{Key => Value1, _ => _}
) -> #{Key := Value2 | Init, _ => _}.
update_with(Key, Fun, Init, Map) when is_function(Fun, 1) andalso is_map(Map) ->
    case Map of
        #{Key := Value} -> Map#{Key := Fun(Value)};
        #{} -> Map#{Key => Init}
    end;
update_with(_Key, _Fun, _Init, Map) when not is_map(Map) ->
    error({badmap, Map});
update_with(_Key, _Fun, _Init, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Keys    the list of keys to keep
%% @param   Map1    the map from which to select entries
%% @returns a new map containing only those entries from `Map1' whose keys
%%          appear in `Keys'.
%% @doc Returns a new map containing only the entries from `Map1' whose key
%% is present in `Keys'.
%%
%% This function raises a `{badmap, Map}' error if `Map1' is not a map, and a
%% `badarg' error if `Keys' is not a list.
%% @end
%%-----------------------------------------------------------------------------
-spec with(Keys :: [K], Map1 :: #{K => V, _ => _}) -> #{K => V}.
with(Keys, Map) when is_list(Keys) andalso is_map(Map) ->
    with_1(Keys, Map, ?MODULE:new());
with(_Keys, Map) when not is_map(Map) ->
    error({badmap, Map});
with(_Keys, _Map) ->
    error(badarg).

%%-----------------------------------------------------------------------------
%% @param   Keys    the list of keys to drop
%% @param   Map1    the map from which to drop entries
%% @returns a new map containing the entries from `Map1' whose keys are not
%%          in `Keys'.
%% @doc Returns a new map containing the entries of `Map1' with the keys in
%% `Keys' removed.
%%
%% This function raises a `{badmap, Map}' error if `Map1' is not a map, and a
%% `badarg' error if `Keys' is not a list.
%% @end
%%-----------------------------------------------------------------------------
-spec without(Keys :: [K], Map1 :: #{K => _, _ => _}) -> #{_ => _}.
without(Keys, Map) when is_list(Keys) andalso is_map(Map) ->
    lists:foldl(fun maps:remove/2, Map, Keys);
without(_Keys, Map) when not is_map(Map) ->
    error({badmap, Map});
without(_Keys, _Map) ->
    error(badarg).

%%
%% Internal functions
%%

%% @private
with_1([], _Map, Acc) ->
    Acc;
with_1([K | Ks], Map, Acc) ->
    case Map of
        #{K := V} -> with_1(Ks, Map, Acc#{K => V});
        #{} -> with_1(Ks, Map, Acc)
    end.

%% @private
iterate_keys(none, undefined, Accum) ->
    lists:reverse(Accum);
iterate_keys(none, ordered, Accum) ->
    lists:sort(Accum);
iterate_keys(none, reversed, Accum) ->
    lists:reverse(lists:sort(Accum));
iterate_keys(none, F, Accum) ->
    lists:sort(F, Accum);
iterate_keys({Key, _Value, Iterator}, Order, Accum) ->
    iterate_keys(maps:next(Iterator), Order, [Key | Accum]).

%% @private
iterate_values(none, Accum) ->
    lists:reverse(Accum);
iterate_values({_Key, Value, Iterator}, Accum) ->
    iterate_values(maps:next(Iterator), [Value | Accum]).

%% @private
iterate_entries(none, Accum) ->
    lists:reverse(Accum);
iterate_entries({Key, Value, Iterator}, Accum) ->
    iterate_entries(maps:next(Iterator), [{Key, Value} | Accum]).

%% @private
iterate_filter(_Pred, none, Accum) ->
    Accum;
iterate_filter(Pred, {Key, Value, Iterator}, Accum) ->
    NewAccum =
        case Pred(Key, Value) of
            true ->
                Accum#{Key => Value};
            _ ->
                Accum
        end,
    iterate_filter(Pred, maps:next(Iterator), NewAccum).

%% @private
iterate_fold(_Fun, none, Accum) ->
    Accum;
iterate_fold(Fun, {Key, Value, Iterator}, Accum) ->
    NewAccum = Fun(Key, Value, Accum),
    iterate_fold(Fun, maps:next(Iterator), NewAccum).

%% @private
iterate_foreach(_Fun, none) ->
    ok;
iterate_foreach(Fun, {Key, Value, Iterator}) ->
    _ = Fun(Key, Value),
    iterate_foreach(Fun, maps:next(Iterator)).

%% @private
iterate_map(_Fun, none, Accum) ->
    Accum;
iterate_map(Fun, {Key, Value, Iterator}, Accum) ->
    NewAccum = Accum#{Key => Fun(Key, Value)},
    iterate_map(Fun, maps:next(Iterator), NewAccum).

%% @private
iterate_merge_with(_Combiner, none, Accum) ->
    Accum;
iterate_merge_with(Combiner, {Key, Value1, Iterator}, Accum) ->
    case Accum of
        #{Key := Value2} ->
            iterate_merge_with(Combiner, maps:next(Iterator), Accum#{
                Key := Combiner(Key, Value1, Value2)
            });
        #{} ->
            iterate_merge_with(Combiner, maps:next(Iterator), Accum#{Key => Value1})
    end.

%% @private
iterate_merge(none, Accum) ->
    Accum;
iterate_merge({Key, Value, Iterator}, Accum) ->
    iterate_merge(maps:next(Iterator), Accum#{Key => Value}).

%% @private
iterate_remove(_Key, none, Accum) ->
    Accum;
iterate_remove(Key, {Key, _Value, Iterator}, Accum) ->
    iterate_remove(Key, maps:next(Iterator), Accum);
iterate_remove(Key, {OtherKey, Value, Iterator}, Accum) ->
    iterate_remove(Key, maps:next(Iterator), Accum#{OtherKey => Value}).

%% @private
iterate_from_list([], Accum) ->
    Accum;
iterate_from_list([{Key, Value} | T], Accum) ->
    iterate_from_list(T, Accum#{Key => Value});
iterate_from_list(_List, _Accum) ->
    error(badarg).

%% @private
iterate_filtermap(_Fun, none, Accum) ->
    lists:reverse(Accum);
iterate_filtermap(Fun, {Key, Value, Iterator}, Accum) ->
    NewAccum =
        case Fun(Key, Value) of
            true ->
                [{Key, Value} | Accum];
            {true, NewValue} ->
                [{Key, NewValue} | Accum];
            false ->
                Accum
        end,
    iterate_filtermap(Fun, maps:next(Iterator), NewAccum).

%% @private
groups_from_list_1(_KeyFun, _ValueFun, [], Acc) ->
    Acc;
groups_from_list_1(KeyFun, ValueFun, [Elem | Rest], Acc) ->
    Key = KeyFun(Elem),
    Value = ValueFun(Elem),
    NewAcc =
        case Acc of
            #{Key := Values} ->
                Acc#{Key := [Value | Values]};
            #{} ->
                Acc#{Key => [Value]}
        end,
    groups_from_list_1(KeyFun, ValueFun, Rest, NewAcc).

%% @private
intersect_with_small_map_first(Combiner, SmallMap, BigMap) ->
    Next = maps:next(maps:iterator(SmallMap)),
    intersect_with_iterate(Next, [], BigMap, Combiner).

%% @private
intersect_with_iterate({K, V1, Iterator}, Keep, BigMap, Combiner) ->
    Next = maps:next(Iterator),
    case BigMap of
        #{K := V2} ->
            V = Combiner(K, V1, V2),
            intersect_with_iterate(Next, [{K, V} | Keep], BigMap, Combiner);
        #{} ->
            intersect_with_iterate(Next, Keep, BigMap, Combiner)
    end;
intersect_with_iterate(none, Keep, _BigMap, _Combiner) ->
    maps:from_list(Keep).

%% @private
is_iterator_valid_1(none) ->
    true;
is_iterator_valid_1({_, _, Iter}) ->
    is_iterator_valid_1(Iter);
is_iterator_valid_1([Pos | Map]) when is_integer(Pos), is_map(Map) ->
    %% Default iterator - try to use it
    _ = maps:next([Pos | Map]),
    true;
is_iterator_valid_1([Keys | Map]) when is_list(Keys), is_map(Map) ->
    %% Ordered iterator - try to use it
    _ = maps:next([Keys | Map]),
    true;
is_iterator_valid_1(_) ->
    false.
