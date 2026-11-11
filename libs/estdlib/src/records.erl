%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
%% @doc Reflection over OTP-29 native records.
%%
%% Mirrors the subset of the OTP-29 `records' module specified in
%% [EEP-0079](https://www.erlang.org/eeps/eep-0079) that AtomVM supports
%% today. The functions in this module are intended for debugging,
%% inspecting record values at runtime, and implementing tools such as
%% printers. Production code should prefer the `Expr#Name.Field' syntax
%% for static field access.
%%
%% The `create/4' and `update/4' functions described by the EEP — which
%% build records from data without consulting a loaded definition — are
%% not yet supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-module(records).

-export([
    get/2,
    get_module/1,
    get_name/1,
    get_field_names/1,
    is_exported/1,
    get_definition/2
]).

-export_type([create_options/0]).

-type create_options() :: #{is_exported := boolean()}.

%%-----------------------------------------------------------------------------
%% @param   Key     the field name (an atom)
%% @param   Record  the native record value
%% @returns the value of field `Key' in `Record'
%% @doc     Returns the value associated with field `Key' in native record
%%          `Record'.
%%
%%          Fails with a `badarg' exception if `Record' is not a native
%%          record or if `Key' is not a field of `Record'.
%% @end
%%-----------------------------------------------------------------------------
-spec get(Key :: atom(), Record :: tuple()) -> term().
get(_Key, _Record) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Record  the native record value
%% @returns the module atom in which `Record' is defined
%% @doc     Returns the module in which the given native record is defined.
%%
%%          Fails with a `badarg' exception if `Record' is not a native
%%          record.
%% @end
%%-----------------------------------------------------------------------------
-spec get_module(Record :: tuple()) -> module().
get_module(_Record) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Record  the native record value
%% @returns the record name atom
%% @doc     Returns the name of the given native record.
%%
%%          Fails with a `badarg' exception if `Record' is not a native
%%          record.
%% @end
%%-----------------------------------------------------------------------------
-spec get_name(Record :: tuple()) -> atom().
get_name(_Record) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Record  the native record value
%% @returns the field-name atoms in declaration order
%% @doc     Returns the list of field names of the given native record.
%%
%%          Fails with a `{badrecord, Record}' exception if `Record' is not
%%          a native record.
%% @end
%%-----------------------------------------------------------------------------
-spec get_field_names(Record :: tuple()) -> [atom()].
get_field_names(_Record) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Record  the native record value
%% @returns `true' if `Record' was captured as exported; otherwise `false'
%% @doc     Returns whether the native record value was captured with the
%%          `is_exported' flag set.
%%
%%          Fails with a `{badrecord, Record}' exception if `Record' is not
%%          a native record.
%% @end
%%-----------------------------------------------------------------------------
-spec is_exported(Record :: tuple()) -> boolean().
is_exported(_Record) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      the module in which the record is defined
%% @param   RecordName  the record name
%% @returns the record definition as `{create_options(), [FieldSpec]}'
%% @doc     Returns the definition of the named record.
%%
%%          The second tuple element is a list whose entries are either the
%%          plain field-name atom (no default value) or a `{Name, Default}'
%%          tuple (the field has a default).
%%
%%          Fails with a `badarg' exception if either argument is not an
%%          atom or if no record with the given name is defined in `Module'.
%% @end
%%-----------------------------------------------------------------------------
-spec get_definition(Module :: module(), RecordName :: atom()) ->
    {create_options(), [{atom(), term()} | atom()]}.
get_definition(_Module, _RecordName) ->
    erlang:nif_error(undefined).
