%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(map_comparisons).

-export([start/0]).

start() ->
    A = fact(4, 1, 1),
    B = fact(4, 1.0, 1.0),

    test(not (#{a => A} > #{a => B})) +
        test(not (#{a => A} < #{a => B})) * 2 +
        test(#{a => A} < #{a => B + 1}) * 4 +
        test(#{a => A} < #{a => A + 1}) * 8 +
        test(not (#{a => A} > #{a => A + 1})) * 16 +
        test(not (#{A => a} > #{B => a})) * 32 +
        test(#{A => a} < #{B => a}) * 64 +
        test(not (#{A => a} == #{B => a})) * 128 +
        test(not (#{A => a} =:= #{B => a})) * 256 +
        test(#{a => A} == #{a => B}) * 512 +
        test(not (#{a => A} =:= #{a => B})) * 1024 +
        test((A == B) and not (A =:= B) and is_float(B)) * 2048 +
        test(not (#{#{x => y, A => a} => a} > #{#{x => y, B => a} => a})) * 4096 +
        test(#{#{x => y, A => a} => a} < #{#{x => y, B => a} => a}) * 8192 +
        test(not (#{#{x => y, A => a} => a} == #{#{x => y, B => a} => a})) * 16384 +
        test(not (#{#{x => y, A => a} => a} =:= #{#{x => y, B => a} => a})) * 32768 +
        test(not (#{#{x => y, a => A} => a} > #{#{x => y, a => B} => a})) * 65536 +
        test(#{#{x => y, a => A} => a} < #{#{x => y, a => B} => a}) * 131072 +
        test(not (#{#{x => y, a => A} => a} == #{#{x => y, a => B} => a})) * 262144 +
        test(not (#{#{x => y, a => A} => a} =:= #{#{x => y, a => B} => a})) * 524288 +
        test(#{a => #{x => y, A => a}} < #{a => #{x => y, B => a}}) * 1048576 +
        test(not (#{a => #{x => y, A => a}} > #{a => #{x => y, B => a}})) * 2097152 +
        test(not (#{a => #{x => y, A => a}} == #{a => #{x => y, B => a}})) * 4194304 +
        test(not (#{a => #{x => y, A => a}} =:= #{a => #{x => y, B => a}})) * 8388608 +
        test(not (#{a => A, b => A} > #{a => B, b => B})) * 16777216 +
        test(#{a => A, b => A} == #{a => B, b => B}) * 33554432.

test(true) ->
    1;
test(false) ->
    0.

fact(N, _D, Acc) when N < 1 ->
    Acc;
fact(N, D, Acc) ->
    fact(N - D, D, Acc * N).
