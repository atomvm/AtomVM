%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
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
%% @doc JSON specific APIs
%%
%% This module contains functions for working with json data.
%% @end
%%-----------------------------------------------------------------------------
-module(json_encoder).
-export([encode/1]).

%%-----------------------------------------------------------------------------
%% @param   Data data to encode to json
%% @returns JSON encoded data
%% @doc Convert data to json encoded binary
%% @end
%%-----------------------------------------------------------------------------
-spec encode(Data :: any()) -> binary().
encode(false) ->
    <<"false">>;
encode(true) ->
    <<"true">>;
encode(nil) ->
    <<"nil">>;
encode(null) ->
    <<"null">>;
encode(undefined) ->
    <<"null">>;
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
    encode(Tail, [Acc | Encoded]);
encode([_V | _T] = L, []) ->
    encode(L, $[);
encode([Value | []], Acc) ->
    Encoded = [encode(Value), $]],
    [Acc | Encoded];
encode([Value | Tail], Acc) ->
    Encoded = [encode(Value), $,],
    encode(Tail, [Acc | Encoded]).

encode_key(Key) when is_atom(Key) ->
    erlang:atom_to_binary(Key, latin1);
encode_key(Key) when is_binary(Key) ->
    Key.
