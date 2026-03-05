%
% This file is part of AtomVM.
%
% Copyright 2024 Fred Dushin <fred@dushin.net>
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
%% @doc A limited implementation of the Erlang/OTP `ets' module.
%% @end
%%-----------------------------------------------------------------------------
-module(ets).

-export([
    new/2,
    lookup/2,
    lookup_element/3,
    lookup_element/4,
    member/2,
    insert/2,
    insert_new/2,
    update_element/3,
    update_element/4,
    update_counter/3,
    update_counter/4,
    take/2,
    delete/1,
    delete/2,
    delete_object/2
]).

-export_type([
    table/0,
    options/0,
    table_type/0,
    access_type/0,
    update_op/0
]).

-opaque table() :: atom | reference().
-type table_type() :: set | bag | duplicate_bag.
-type access_type() :: private | protected | public.
-type option() :: table_type() | {keypos, non_neg_integer()} | access_type().
-type options() :: [option()].
-type update_op() ::
    {pos_integer(), integer()} | {pos_integer(), integer(), integer(), integer()}.

%%-----------------------------------------------------------------------------
%% @param   Name the ets table name
%% @param   Options the options used to create the table
%% @returns A new ets table
%% @doc Create a new ets table.
%%
%% Supported table types are `set', `bag', and `duplicate_bag'.
%% The `ordered_set' type is not currently supported.
%% @end
%%-----------------------------------------------------------------------------
-spec new(Name :: atom(), Options :: options()) -> table().
new(_Name, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries
%% @returns a list of matching tuples, or an empty list if none found
%% @doc Look up an entry in an ets table.
%%
%% For `set' tables, returns at most one element. For `bag' and `duplicate_bag'
%% tables, returns all objects with the matching key.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup(Table :: table(), Key :: term()) -> [tuple()].
lookup(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries
%% @param   Pos index of the element to retrieve (1-based)
%% @returns the element at position Pos from the matching tuple, or a list of
%%          such elements if the table is of type `bag' or `duplicate_bag'
%% @doc Look up an element from an entry in an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_element(Table :: table(), Key :: term(), Pos :: pos_integer()) -> term() | [term()].
lookup_element(_Table, _Key, _Pos) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries
%% @param   Pos index of the element to retrieve (1-based)
%% @param   Default value returned if the key does not exist
%% @returns the element at position Pos from the matching tuple, or a list of
%%          such elements if the table is of type `bag' or `duplicate_bag',
%%          or Default if the key does not exist
%% @doc Look up an element from an entry in an ets table with a default value.
%%
%% Unlike `lookup_element/3', returns Default instead of raising `badarg' when
%% the key does not exist.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_element(Table :: table(), Key :: term(), Pos :: pos_integer(), Default :: term()) ->
    term() | [term()].
lookup_element(_Table, _Key, _Pos, _Default) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key to check for existence
%% @returns true if the key exists in the table; false otherwise
%% @doc Check if a key exists in an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec member(Table :: table(), Key :: term()) -> boolean().
member(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Entry the entry or list of entries to insert
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Insert an entry into an ets table.
%%
%% For `set' tables, an existing entry with the same key is overwritten.
%% For `bag' tables, the object is added unless an identical object already
%% exists. For `duplicate_bag' tables, the object is always added.
%% The operation is atomic.
%% @end
%%-----------------------------------------------------------------------------

-spec insert(Table :: table(), Entry :: tuple() | [tuple()]) -> true.
insert(_Table, _Entry) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Entry the entry or list of entries to insert
%% @returns true if all entries were inserted; false if any key already exists
%% @doc Insert an entry into an ets table only if the key does not already exist.
%% @end
%%-----------------------------------------------------------------------------
-spec insert_new(Table :: table(), Entry :: tuple() | [tuple()]) -> boolean().
insert_new(_Table, _Entry) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry to update
%% @param   ElementSpec a tuple {Pos, Value} or a list of such tuples, specifying
%%          the position(s) (1-based) and new value(s) to set
%% @returns true if the entry was updated; false if the key does not exist
%% @doc Update one or more elements of an existing entry in an ets table.
%%
%% The key field itself cannot be updated. Returns `false' if no entry with
%% the given key exists.
%% @end
%%-----------------------------------------------------------------------------
-spec update_element(
    Table :: table(),
    Key :: term(),
    ElementSpec :: {pos_integer(), term()} | [{pos_integer(), term()}]
) -> boolean().
update_element(_Table, _Key, _ElementSpec) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry to update
%% @param   ElementSpec a tuple {Pos, Value} or a list of such tuples, specifying
%%          the position(s) (1-based) and new value(s) to set
%% @param   Default a default tuple to insert if the key does not exist
%% @returns true if the entry was updated or inserted; false if insertion failed
%% @doc Update one or more elements of an existing entry, inserting Default if missing.
%%
%% If no entry with the given key exists, inserts Default into the table,
%% then applies the element updates.
%% @end
%%-----------------------------------------------------------------------------
-spec update_element(
    Table :: table(),
    Key :: term(),
    ElementSpec :: {pos_integer(), term()} | [{pos_integer(), term()}],
    Default :: tuple()
) -> boolean().
update_element(_Table, _Key, _ElementSpec, _Default) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry expecting to contain a tuple
%%          of integers or a single integer
%% @param   Params an integer increment, a single update operation, or a list
%%          of update operations. An update operation is a tuple
%%          `{Pos, Increment}' or `{Pos, Increment, Threshold, SetValue}',
%%          where Pos is a 1-based index.
%% @returns the new counter value, or a list of new values when Params is a list
%% @doc Updates one or more counter values at Key in the table.
%% @end
%%-----------------------------------------------------------------------------
-spec update_counter(
    Table :: table(),
    Key :: term(),
    Params :: integer() | update_op() | [update_op()]
) -> integer() | [integer()].
update_counter(_Table, _Key, _Params) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up the entry expecting to contain a tuple
%%         of integers or a single integer
%% @param   Params an integer increment, a single update operation, or a list
%%          of update operations (see `update_counter/3' for the format)
%% @param   Default a default object (tuple) to insert if the key does not
%%          exist, after which the update operation is applied to it
%% @returns the new counter value, or a list of new values when Params is a list
%% @doc Updates one or more counter values at Key in the table.
%%
%% Equivalent to `update_counter/3', but inserts Default as a new entry if
%% no object with Key exists, then performs the counter update on it.
%% @end
%%-----------------------------------------------------------------------------
-spec update_counter(
    Table :: table(),
    Key :: term(),
    Params :: integer() | update_op() | [update_op()],
    Default :: tuple()
) -> integer() | [integer()].
update_counter(_Table, _Key, _Params, _Default) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to look up and remove entries
%% @returns a list of the removed objects, or an empty list if none found
%% @doc Return and delete all entries with the given key from an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec take(Table :: table(), Key :: term()) -> [tuple()].
take(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @returns true;
%% @doc Delete an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Table :: table()) -> true.
delete(_Table) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries to delete
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Delete all entries with the given key from an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Table :: table(), Key :: term()) -> true.
delete(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Object the exact object to delete
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Delete a specific object from an ets table.
%%
%% Unlike `delete/2', which deletes all entries matching a key, this function
%% deletes only entries that exactly match the given object. For `bag' tables,
%% other objects sharing the same key are left intact. For `duplicate_bag'
%% tables, all instances of the identical object are removed.
%% @end
%%-----------------------------------------------------------------------------
-spec delete_object(Table :: table(), Object :: tuple()) -> true.
delete_object(_Table, _Object) ->
    erlang:nif_error(undefined).
