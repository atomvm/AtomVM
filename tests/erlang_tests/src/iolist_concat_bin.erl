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

-module(iolist_concat_bin).

-export([start/0, id/1]).

start() ->
    L1 = [id(<<"Ato">>), id($m) | id(<<"VM">>)],
    L2 = [id(<<"Ato">>), {id($m)} | id(<<"VM">>)],
    Size = erlang:iolist_size(L1),
    Expected = <<"AtomVM">>,
    bool_to_int(try_to_bin(L1) == Expected) * 1000 + Size +
        try_to_bin(L2) * 10000 + try_to_bin({L1}) * 20000 +
        bool_to_int(try_to_bin([]) == <<"">>) * 40000 + erlang:iolist_size(id([])).

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.

id(I) when is_integer(I) or is_binary(I) or is_list(I) ->
    I.

try_to_bin(L) when is_list(L) or is_tuple(L) ->
    try erlang:iolist_to_binary(L) of
        Res -> Res
    catch
        error:badarg -> 1;
        _:_ -> -2
    end.
