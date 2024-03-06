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
    insert/2,
    lookup/2,
    delete/2
]).

-export_type([
    table/0,
    options/0,
    table_type/0,
    access_type/0
]).

-opaque table() :: atom | reference().
-type table_type() :: set.
-type access_type() :: private | protected | public.
-type option() :: table_type() | {keypos, non_neg_integer()} | access_type().
-type options() :: [option()].

%%-----------------------------------------------------------------------------
%% @param   Name the ets table name
%% @param   Options the options used to create the table
%% @returns A new ets table
%% @doc Create a new ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec new(Name :: atom(), Options :: options()) -> table().
new(_Name, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Entry the entry to insert
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Insert an entry into an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec insert(Table :: table(), Entry :: tuple()) -> true.
insert(_Table, _Entry) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries
%% @returns the entry in a set, or a list of entries, if the table permits
%% @doc Look up an entry in an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec lookup(Table :: table(), Key :: term()) -> [tuple()].
lookup(_Table, _Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Table a reference to the ets table
%% @param   Key the key used to lookup one or more entries to delete
%% @returns true; otherwise, an error is raised if arguments are bad
%% @doc Delete an entry from an ets table.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Table :: table(), Key :: term()) -> true.
delete(_Table, _Key) ->
    erlang:nif_error(undefined).
