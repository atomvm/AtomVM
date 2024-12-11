%
% This file is part of AtomVM.
%
% Copyright 2025 Tomasz Sobkiewicz <tomasz.sobkiewicz@swmansion.com>
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

-module(test_file).
-export([start/0, test/0]).

start() ->
    test().

test() ->
    Res = file:native_name_encoding(),
    ok = is_proper_encoding(Res),
    ok = test_get_cwd(),
    ok.

is_proper_encoding(utf8) ->
    ok;
is_proper_encoding(latin1) ->
    ok;
is_proper_encoding(_) ->
    error.

test_get_cwd() ->
    {ok, Path} = file:get_cwd(),
    CanValidate = erlang:system_info(machine) == "BEAM" orelse atomvm:platform() == generic_unix,
    if
        CanValidate -> validate_path(Path);
        true -> ok
    end.

validate_path("/" ++ _Rest) -> ok.
