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

-module(jsonish_encode).

-export([start/0, encode/1]).

start() ->
    L = encode([
        {<<"data">>, [
            {float, 1.5},
            {string, <<"abc">>},
            {int, 42}
        ]},
        {test, true}
    ]),
    Size = erlang:iolist_size(L),
    Expected = <<"{\"data\":{\"float\":1.5,\"string\":\"abc\",\"int\":42},\"test\":true}">>,
    bool_to_int(erlang:iolist_to_binary(L) == Expected) * 1000 + Size.

bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0;
bool_to_int(_) ->
    -1.

encode(true) ->
    <<"true">>;
encode(false) ->
    <<"false">>;
encode(Value) when is_atom(Value) ->
    [$", erlang:atom_to_binary(Value, latin1), $"];
encode(Value) when is_binary(Value) ->
    [$", Value, $"];
encode(Value) when is_float(Value) ->
    erlang:float_to_binary(Value, [{decimals, 32}, compact]);
encode(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);
encode(V) ->
    encode(V, []).

encode([{_K, _V} | _T] = L, []) ->
    encode(L, ${);
encode([{Key, Value} | []], Acc) ->
    Encoded = [$", encode_key(Key), "\":", encode(Value), $}],
    [Acc | Encoded];
encode([{Key, Value} | Tail], Acc) ->
    Encoded = [$", encode_key(Key), "\":", encode(Value), $,],
    encode(Tail, [Acc | Encoded]).

encode_key(Key) when is_atom(Key) ->
    erlang:atom_to_binary(Key, latin1);
encode_key(Key) when is_binary(Key) ->
    Key.
