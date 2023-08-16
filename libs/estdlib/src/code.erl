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

-export([load_abs/1, load_binary/3]).

%%-----------------------------------------------------------------------------
%% @param   Filename    path to the beam to open, without .beams suffix
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
