%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(binary_is_iolist).

-export([start/0, id/1, try_size/1, try_to_bin/1]).

start() ->
    Size = try_size(id(<<"AtomVM">>)),
    Expected = <<"AtomVM">>,
    bool_to_int(try_to_bin(id(<<"AtomVM">>)) == Expected) * 1000 + Size.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.

id(I) when is_integer(I) or is_binary(I) or is_list(I) ->
    I.

try_to_bin(L) when is_list(L) or is_binary(L) ->
    try id(erlang:iolist_to_binary(id(L))) of
        Res -> Res
    catch
        error:badarg -> -1;
        _:_ -> -2
    end.

try_size(L) when is_list(L) or is_binary(L) ->
    try id(erlang:iolist_size(id(L))) of
        Res -> Res
    catch
        error:badarg -> -10;
        _:_ -> -20
    end.
