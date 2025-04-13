%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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
%% @doc An implementation of a subset of the Erlang/OTP code interface.
%% @end
%%-----------------------------------------------------------------------------
-module(code).

-export([
    all_available/0,
    all_loaded/0,
    load_abs/1,
    load_binary/3,
    ensure_loaded/1,
    which/1,
    is_loaded/1
]).

%%-----------------------------------------------------------------------------
%% @returns A list of available modules, including loaded modules
%% @doc     Return all modules available from loaded avm packs, in addition
%%          to loaded modules. List of available modules may be incomplete if
%%          this function is called while a module is loaded.
%%          Result type differs from Erlang/OTP: names of modules is a binary
%%          (and not a string), and second term of tuples is currently
%%          unspecified
%% @end
%%-----------------------------------------------------------------------------
-spec all_available() -> [{unicode:unicode_binary(), term(), boolean()}].
all_available() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A list of all loaded modules
%% @doc     Return a list of all loaded modules.
%%          Result type differs from Erlang/OTP: second term of tuples is
%%          currently unspecified
%% @end
%%-----------------------------------------------------------------------------
-spec all_loaded() -> [{atom(), term()}].
all_loaded() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Filename    path to the beam to open, without .beam suffix
%% @returns A tuple with the name of the module
%% @doc     Load a module from a path.
%% Error return result type is different from Erlang/OTP.
%% @end
%%-----------------------------------------------------------------------------
-spec load_abs(Filename :: string()) -> error | {module, module()}.
load_abs(_Filename) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      name of the module to load
%% @param   Filename    path to the beam (unused)
%% @param   Binary      binary of the module to load
%% @returns A tuple with the name of the module
%% @doc     Load a module from a binary.
%% Error return result type is different from Erlang/OTP.
%% Also unlike Erlang/OTP, no check is performed to verify that `Module'
%% matches the name of the loaded module.
%% @end
%%-----------------------------------------------------------------------------
-spec load_binary(Module :: module(), Filename :: string(), Binary :: binary()) ->
    error | {module, module()}.
load_binary(_Module, _Filename, _Binary) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      module to load
%% @returns Tuple `{module, Module}' if module is loaded or `{error, embedded}'
%% @doc     Try to load a module if it's not already loaded. AtomVM works in
%% an embedded-like mode where modules are loaded at start-up but modules
%% can be loaded explicitely as well (especially from a binary with `load_binary/3').
%% So this function can be used to determine if a module is loaded.
%% It is called by Elixir Code module.
%% @end
%%-----------------------------------------------------------------------------
-spec ensure_loaded(Module) -> {module, Module} | {error, embedded | any()} when
    Module :: atom().
ensure_loaded(_Module) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      module to test
%% @returns Tuple `{file, preloaded}' if module is loaded or `false'
%% @doc     Determine if a module is loaded. AtomVM works in
%% an embedded-like mode where modules are loaded at start-up but modules
%% can be loaded explicitely as well (especially from a binary with `load_binary/3').
%% @end
%%-----------------------------------------------------------------------------
is_loaded(Module) ->
    case ?MODULE:ensure_loaded(Module) of
        {module, _Module} ->
            {file, preloaded};
        {error, _} ->
            false
    end.

%%-----------------------------------------------------------------------------
%% @param   Module      module to test
%% @returns `preloaded' if module is loaded or `false'
%% @doc     Determine if a module is loaded. There currently is no way to
%% distinguish a module that was loaded with `load_binary/3' or that was
%% preloaded at startup.
%% @end
%%-----------------------------------------------------------------------------
which(Module) ->
    case ?MODULE:ensure_loaded(Module) of
        {module, _Module} ->
            preloaded;
        {error, _} ->
            non_existing
    end.
