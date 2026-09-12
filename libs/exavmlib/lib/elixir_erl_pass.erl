%
% This file is part of elixir-lang.
%
% Copyright 2021 The Elixir Team
% Copyright 2012 Plataformatec
% https://github.com/elixir-lang/elixir/blob/v1.20.0-rc.6/lib/elixir/src/elixir_erl_pass.erl
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
% SPDX-License-Identifier: Apache-2.0
%
% Adapted for AtomVM:
% - Only no_parens_remote/2 and parens_map_field/2 are kept. The rest of the
%   module is the Elixir compiler itself and never runs on a device.
% - The deprecation notice of both is dropped, it calls IO.warn_once/3 and
%   elixir_aliases:inspect/1, neither of which AtomVM has. That leaves
%   parens_map_field/2 with no use for its first argument.
% - Reformatted with erlfmt, and given specs and edoc.
%

%%-----------------------------------------------------------------------------
%% @doc Runtime helpers the Elixir compiler emits calls to.
%%
%% elixirc compiles `Map.field' to an inline lookup plus a call to
%% `no_parens_remote/2' on the branch taken when the term is not a map or the
%% key is missing, and the deprecated `Map.field()' to the same lookup followed
%% by `parens_map_field/2'. Without this module both spellings raise `undef'
%% where the BEAM raises `KeyError' or `BadMapError'.
%% @end
%%-----------------------------------------------------------------------------

-module(elixir_erl_pass).

-export([no_parens_remote/2, parens_map_field/2]).

%%-----------------------------------------------------------------------------
%% @param   Term the term the field was read from
%% @param   Key the field name
%% @returns `{ok, Value}' when `Term' is a module, the error to raise otherwise
%% @doc     Answer a dot access that the inline map lookup did not resolve.
%%
%%          An atom other than `nil', `true' and `false' is a module, and the
%%          access is the deprecated no parentheses form of `Term:Key()'.
%% @end
%%-----------------------------------------------------------------------------
-spec no_parens_remote(Term :: term(), Key :: atom()) ->
    {ok, term()} | {error, {badmap, term()} | {badkey, atom(), map()}}.
no_parens_remote(nil, _Key) ->
    {error, {badmap, nil}};
no_parens_remote(false, _Key) ->
    {error, {badmap, false}};
no_parens_remote(true, _Key) ->
    {error, {badmap, true}};
no_parens_remote(Atom, Fun) when is_atom(Atom) ->
    {ok, apply(Atom, Fun, [])};
no_parens_remote(#{} = Map, Key) ->
    {error, {badkey, Key, Map}};
no_parens_remote(Other, _Key) ->
    {error, {badmap, Other}}.

%%-----------------------------------------------------------------------------
%% @param   Key the field name
%% @param   Value the value the inline map lookup returned
%% @returns `Value'
%% @doc     Answer the deprecated `Map.field()' form, which reads a field.
%% @end
%%-----------------------------------------------------------------------------
-spec parens_map_field(Key :: atom(), Value :: term()) -> term().
parens_map_field(_Key, Value) ->
    Value.
