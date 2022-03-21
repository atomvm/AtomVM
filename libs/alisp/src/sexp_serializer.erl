%
% This file is part of AtomVM.
%
% Copyright 2020-2021 Davide Bettio <davide@uninstall.it>
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

-module(sexp_serializer).

-export([serialize/1]).

serialize(Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
serialize(Value) when is_binary(Value) ->
    [$", Value, $"];
serialize(Value) when is_float(Value) ->
    float_to_binary(Value);
serialize(Value) when is_integer(Value) ->
    integer_to_binary(Value);
serialize(Value) when is_pid(Value) ->
    [<<"(pid \"">>, pid_to_list(Value), <<"\")">>];
serialize(Value) when is_reference(Value) ->
    [<<"(ref \"">>, ref_to_list(Value), <<"\")">>];
serialize(Value) when is_tuple(Value) ->
    [<<"(tuple ">>, serialize_list(tuple_to_list(Value)), <<")">>];
serialize(L) ->
    [$(, serialize_list(L), $)].

serialize_list([]) ->
    [];
serialize_list([I]) ->
    [serialize(I)];
serialize_list([H | T]) ->
    [serialize(H), $\s | serialize_list(T)].
