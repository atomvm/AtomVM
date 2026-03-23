%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_float_short).

-export([start/0]).

start() ->
    ok = test_short_uses_shortest_format(),
    ok.

test_short_uses_shortest_format() ->
    case is_float32_fallback() of
        true ->
            1.2e20 = erlang:binary_to_float(erlang:float_to_binary(1.2e20, [short])),
            1.2e-20 = erlang:binary_to_float(erlang:float_to_binary(1.2e-20, [short])),
            1.2e20 = erlang:list_to_float(erlang:float_to_list(1.2e20, [short])),
            1.2e-20 = erlang:list_to_float(erlang:float_to_list(1.2e-20, [short]));
        false ->
            % These values distinguish shortest formatting from fixed-decimal fallback.
            <<"1.2e20">> = erlang:float_to_binary(1.2e20, [short]),
            <<"1.2e-20">> = erlang:float_to_binary(1.2e-20, [short]),
            "1.2e20" = erlang:float_to_list(1.2e20, [short]),
            "1.2e-20" = erlang:float_to_list(1.2e-20, [short])
    end,
    ok.

is_float32_fallback() ->
    erlang:system_info(avm_floatsize) =:= 4 andalso
        erlang:float_to_binary(0.1, [short]) =/= <<"0.1">>.
