%
% This file is part of AtomVM.
%
% Copyright 2024 Jakub Gonet <jakub.gonet@swmansion.com>
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

-module(test_fun_info).

-export([start/0, get_fun/1]).

-define(SUCCESS, 0).
-define(ERROR, 1).

start() ->
    try test_funs() of
        ok ->
            ?SUCCESS
    catch
        _:E:S ->
            erlang:display({E, S}),
            ?ERROR
    end.

f(_X, _Y, _Z) ->
    ok.

get_fun(local) ->
    fun(B) -> not B end;
get_fun(local_ref) ->
    fun f/3;
get_fun(external_ref) ->
    fun erlang:apply/2;
get_fun(not_existing_ref) ->
    fun erlang:undef/8.

test_funs() ->
    LocalFun = ?MODULE:get_fun(local),
    LocalFunRef = ?MODULE:get_fun(local_ref),
    ExternalFunRef = ?MODULE:get_fun(external_ref),
    NotExistingFunRef = ?MODULE:get_fun(not_existing_ref),

    {module, test_fun_info} = erlang:fun_info(LocalFun, module),
    {name, LocalFunName} = erlang:fun_info(LocalFun, name),
    % e.g. -get_fun/1-fun-1-
    true = atom_contains(LocalFunName, "get_fun"),
    {arity, 1} = erlang:fun_info(LocalFun, arity),
    {type, local} = erlang:fun_info(LocalFun, type),
    % TODO: implement env: env is mocked here and always return []
    {env, []} = erlang:fun_info(LocalFun, env),

    {module, test_fun_info} = erlang:fun_info(LocalFunRef, module),
    {name, LocalFunRefName} = erlang:fun_info(LocalFunRef, name),
    % across Erlang versions, this representation changed frequently
    Format1 = atom_contains(LocalFunRefName, "get_fun"),
    Format2 = atom_contains(LocalFunRefName, "f/3"),
    Format3 = LocalFunRefName == f,
    true = Format1 or Format2 or Format3,
    {arity, 3} = erlang:fun_info(LocalFunRef, arity),
    {type, local} = erlang:fun_info(LocalFunRef, type),
    % TODO: implement env: env is mocked here and always return []
    {env, []} = erlang:fun_info(LocalFunRef, env),

    {module, erlang} = erlang:fun_info(ExternalFunRef, module),
    {name, apply} = erlang:fun_info(ExternalFunRef, name),
    {arity, 2} = erlang:fun_info(ExternalFunRef, arity),
    {type, external} = erlang:fun_info(ExternalFunRef, type),
    {env, []} = erlang:fun_info(ExternalFunRef, env),

    {module, erlang} = erlang:fun_info(NotExistingFunRef, module),
    {name, undef} = erlang:fun_info(NotExistingFunRef, name),
    {arity, 8} = erlang:fun_info(NotExistingFunRef, arity),
    {type, external} = erlang:fun_info(NotExistingFunRef, type),
    % TODO: implement env: env is mocked here and always return []
    {env, []} = erlang:fun_info(NotExistingFunRef, env),

    ok.

atom_contains(Atom, Pattern) when is_atom(Atom) ->
    atom_contains(atom_to_list(Atom), Pattern);
atom_contains([_C | Rest] = String, Pattern) ->
    case prefix_match(String, Pattern) of
        true ->
            true;
        false ->
            atom_contains(Rest, Pattern)
    end;
atom_contains([], _Pattern) ->
    false.

prefix_match(_StringTail, []) ->
    true;
prefix_match([Char | StringTail], [Char | PatternTail]) ->
    prefix_match(StringTail, PatternTail);
prefix_match(_String, _Pattern) ->
    false.
