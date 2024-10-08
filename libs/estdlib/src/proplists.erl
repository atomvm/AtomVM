%
% This file is part of AtomVM.
%
% Copyright 2018-2023 Fred Dushin <fred@dushin.net>
% Copyright 2000-2003 Richard Carlsson <carlsson.richard@gmail.com>
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
%% @doc An implementation of the Erlang/OTP proplists interface.
%%
%% This module implements a strict subset of the Erlang/OTP proplists
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(proplists).

-export([
    property/1, property/2,
    compact/1,
    unfold/1,
    delete/2,
    get_bool/2,
    get_all_values/2,
    get_value/2, get_value/3,
    is_defined/2,
    lookup/2,
    lookup_all/2,
    from_map/1,
    to_map/1
]).

-export_type([property/0, proplist/0]).

-type property() :: atom() | {term(), term()}.
-type proplist() :: [property()].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   PropertyIn a property
%% @returns the same property in normal form
%% @doc     Creates a normal form (minimal) representation of a property. If `PropertyIn' is
%%          `{Key, true}', where `Key' is an atom, `Key' is returned, otherwise the whole
%%          term `PropertyIn' is returned.
%%          See also `property/2'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec property(PropertyIn) -> PropertyOut when
    PropertyIn :: property(),
    PropertyOut :: property().

property({Key, true}) when is_atom(Key) ->
    Key;
property(Property) ->
    Property.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the property key
%% @param   Value the property value
%% @returns Creates a property in normal form
%% @doc     Creates a normal form (minimal) representation of a simple key/value property.
%%          Returns `Key' if `Value' is `true' and `Key' is an atom, otherwise a tuple
%%          `{Key, Value}' is returned.
%%          See also `property/1'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec property(Key, Value) -> Property when
    Key :: term(),
    Value :: term(),
    Property :: atom() | {term(), term()}.

property(Key, true) when is_atom(Key) ->
    Key;
property(Key, Value) ->
    {Key, Value}.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the item key that will be deleted
%% @param   List the property list from which items will be deleted
%% @returns A list without items having key `Key'
%% @doc     Deletes all entries associated with `Key' from `List'.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Key, List) -> List when
    Key :: term(),
    List :: [term()].
delete(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            delete(Key, Ps);
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            delete(Key, Ps);
        true ->
            [P | delete(Key, Ps)]
    end;
delete(_, []) ->
    [].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key that will be searched
%% @param   List the list where key is searched
%% @returns `true' if `Key' is defined, otherwise false
%% @doc     Returns `true' if `List' contains at least one entry associated with `Key', otherwise
%%          `false'.
%% @end
%%-----------------------------------------------------------------------------
-spec is_defined(Key, List) -> boolean() when
    Key :: term(),
    List :: [term()].

is_defined(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            true;
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            true;
        true ->
            is_defined(Key, Ps)
    end;
is_defined(_Key, []) ->
    false.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key that will be searched
%% @param   List the list where key is searched
%% @returns `true' when exists an option with given key that is `true', otherwise `false'
%% @doc     Returns the value of a boolean key/value option. If
%%          [`lookup(Key, List)'](`lookup/2') would yield `{Key, true}', this function
%%          returns `true', otherwise `false'.
%%          See also `get_value/2', `lookup/2'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_bool(Key, List) -> boolean() when
    Key :: term(),
    List :: [term()].

get_bool(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            true;
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            case P of
                {_, true} ->
                    true;
                _ ->
                    %% Don't continue the search!
                    false
            end;
        true ->
            get_bool(Key, Ps)
    end;
get_bool(_Key, []) ->
    false.

%%-----------------------------------------------------------------------------
%% @equiv   get_value(Key, List, undefined)
%% @doc     Get a value from a property list.
%% @end
%%-----------------------------------------------------------------------------
-spec get_value(Key :: term(), List :: list(property())) -> term() | true | undefined.
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
-spec get_value(Key :: term(), List :: list(property()), Default :: term()) -> term().
get_value(_Key, [], Default) ->
    Default;
get_value(Key, [{Key, Value} | _T], _Default) ->
    Value;
get_value(Key, [Key | _T], _Default) when is_atom(Key) ->
    true;
get_value(Key, [_H | T], Default) ->
    get_value(Key, T, Default).

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the values
%% @param   List the property list from which to get the value
%% @returns a list of values for all entries having `Key' as key
%% @doc     Similar to `get_value/2', but returns the list of values for _all_ entries
%%          `{Key, Value}' in `List'. If no such entry exists, the result is the empty list.
%% @end
%%-----------------------------------------------------------------------------
-spec get_all_values(Key, List) -> [term()] when
    Key :: term(),
    List :: [term()].

get_all_values(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            [true | get_all_values(Key, Ps)];
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            case P of
                {_, Value} ->
                    [Value | get_all_values(Key, Ps)];
                _ ->
                    get_all_values(Key, Ps)
            end;
        true ->
            get_all_values(Key, Ps)
    end;
get_all_values(_Key, []) ->
    [].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the entry
%% @param   List the property list from which to get the entry
%% @returns Either the found entry (always as a tuple) or `none'
%% @doc     Returns the first entry associated with `Key' in `List', if one exists,
%%          otherwise returns `none'. For an atom `A' in the list, the tuple `{A, true}' is
%%          the entry associated with `A'.
%%          See also `get_bool/2', `get_value/2', `lookup_all/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup(Key, List) -> 'none' | tuple() when
    Key :: term(),
    List :: [term()].

lookup(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            {Key, true};
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            %% Note that <code>Key</code> does not have to be an atom in this case.
            P;
        true ->
            lookup(Key, Ps)
    end;
lookup(_Key, []) ->
    none.

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the entries
%% @param   List the property list from which to get the entries
%% @returns all entries having `Key' as key
%% @doc     Returns the list of all entries associated with `Key' in `List'. If no such
%%          entry exists, the result is the empty list.
%%          See also `lookup/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_all(Key, List) -> [tuple()] when
    Key :: term(),
    List :: [term()].

lookup_all(Key, [P | Ps]) ->
    if
        is_atom(P), P =:= Key ->
            [{Key, true} | lookup_all(Key, Ps)];
        tuple_size(P) >= 1, element(1, P) =:= Key ->
            [P | lookup_all(Key, Ps)];
        true ->
            lookup_all(Key, Ps)
    end;
lookup_all(_Key, []) ->
    [].

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   List the list will be converted to a map, such as `[key, {one, 1}]'
%% @returns the list converted as a map, such as `#{key => true, one => 1}'
%% @doc     Converts the property list `List' to a map
%%
%%          Shorthand atom values in `List' will be expanded to an association of the form
%%          `Atom => true'. Tuples of the form `{Key, Value}' in `List' will be converted to
%%          an association of the form `Key => Value'. Anything else will be silently
%%          ignored.
%%
%%          If the same key appears in `List' multiple times, the value of the one appearing
%%          nearest to the head of `List' will be in the result map, that is the value that
%%          would be returned by a call to [`get_value(Key, List)'](`get_value/2').
%% @end
%%-----------------------------------------------------------------------------
-spec to_map(List) -> Map when
    List :: [Shorthand | {Key, Value} | term()],
    Map :: #{Shorthand => 'true', Key => Value},
    Shorthand :: atom(),
    Key :: term(),
    Value :: term().

to_map(List) ->
    lists:foldr(
        fun
            ({K, V}, M) ->
                M#{K => V};
            %% if tuples with arity /= 2 appear before atoms or
            %% tuples with arity == 2, get_value/2,3 returns early
            (T, M) when 1 =< tuple_size(T) ->
                maps:remove(element(1, T), M);
            (K, M) when is_atom(K) ->
                M#{K => true};
            (_, M) ->
                M
        end,
        #{},
        List
    ).

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   Map the map that will be converted, such as `#{key => true}'
%% @returns the map converted to list, such as `[{key, true}]'
%% @doc     Converts the map `Map' to a property list.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec from_map(Map) -> List when
    Map :: #{Key => Value},
    List :: [{Key, Value}],
    Key :: term(),
    Value :: term().

from_map(Map) ->
    maps:to_list(Map).

% Taken from `otp/blob/master/lib/stdlib/src/proplists.erl`
%%-----------------------------------------------------------------------------
%% @param   ListIn the list that will be unfolded, such as `[key]'
%% @returns the unfolded list, such as `{key, true}'
%% @doc     Unfolds all occurrences of atoms in `ListIn' to tuples `{Atom, true}'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec unfold(ListIn) -> ListOut when
    ListIn :: [term()],
    ListOut :: [term()].

unfold([P | Ps]) ->
    if
        is_atom(P) ->
            [{P, true} | unfold(Ps)];
        true ->
            [P | unfold(Ps)]
    end;
unfold([]) ->
    [].

%%-----------------------------------------------------------------------------
%% @param   ListIn the list will be compacted, such as `[{key, true}]'
%% @returns the compacted list, such as `[key]'
%% @doc     Minimizes the representation of all entries in the list. This is equivalent to
%%          `[property(P) || P <- ListIn]'.
%%          See also `property/1', `unfold/1'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec compact(ListIn) -> ListOut when
    ListIn :: [property()],
    ListOut :: [property()].

compact(ListIn) ->
    [property(P) || P <- ListIn].
