%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(decode_int48).

-export([start/0, decode48/1, bin/3, g/1]).

start() ->
    ((decode48(bin(b, u, [16#CA, 16#FE, 16#BA, 16#BE, 16#DE, 16#AD])) * 11) bxor
        (decode48(bin(b, s, [16#CA, 16#FE, 16#BA, 16#BE, 16#DE, 16#AD])) * -3) bxor
        (decode48(bin(l, u, [16#CA, 16#FE, 16#BA, 16#BE, 16#DE, 16#AD])) * 5) bxor
        (decode48(bin(l, s, [16#CA, 16#FE, 16#BA, 16#BE, 16#DE, 16#AD])) * -7)) rem 876113.

decode48(<<0, U48B:48/integer-unsigned-big, 0>>) ->
    U48B;
decode48(<<1, S48B:48/integer-signed-big, 0>>) ->
    S48B;
decode48(<<0, U48L:48/integer-unsigned-little, 1>>) ->
    U48L;
decode48(<<1, S48L:48/integer-signed-little, 1>>) ->
    S48L;
decode48(B) ->
    erlang:binary_to_integer(B).

bin(b, u, L) ->
    g([0 | L ++ [0]]);
bin(b, s, L) ->
    g([1 | L ++ [0]]);
bin(l, u, L) ->
    g([0 | L ++ [1]]);
bin(l, s, L) ->
    g([1 | L ++ [1]]).

g(X) ->
    erlang:list_to_binary(X).
