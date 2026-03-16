%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_crc32).

-export([
    start/0,
    id/1
]).

start() ->
    ok = test_crc32_binary(),
    ok = test_crc32_iolist(),
    ok = test_crc32_empty(),
    ok = test_crc32_2(),
    ok = test_crc32_combine(),
    ok = test_crc32_badarg(),
    ok = test_crc32_boundary(),
    ok = test_crc32_empty_identity(),
    ok = test_crc32_nested_iodata(),
    ok = test_crc32_float_badarg(),
    ok = test_crc32_cross_check(),
    0.

test_crc32_binary() ->
    3964322768 = erlang:crc32(?MODULE:id(<<"Hello, World!">>)),
    907060870 = erlang:crc32(?MODULE:id(<<"hello">>)),
    ok.

test_crc32_iolist() ->
    Crc1 = erlang:crc32(?MODULE:id(<<"Hello, World!">>)),
    Crc2 = erlang:crc32(?MODULE:id([<<"Hello">>, <<", ">>, <<"World!">>])),
    Crc1 = Crc2,

    Crc3 = erlang:crc32(?MODULE:id("hello")),
    Crc4 = erlang:crc32(?MODULE:id(<<"hello">>)),
    Crc3 = Crc4,
    ok.

test_crc32_empty() ->
    0 = erlang:crc32(?MODULE:id(<<>>)),
    0 = erlang:crc32(?MODULE:id([])),
    ok.

test_crc32_2() ->
    Crc1 = erlang:crc32(?MODULE:id(<<"Hello, World!">>)),
    CrcA = erlang:crc32(?MODULE:id(<<"Hello">>)),
    CrcB = erlang:crc32(CrcA, ?MODULE:id(<<", World!">>)),
    Crc1 = CrcB,

    Crc2 = erlang:crc32(0, ?MODULE:id(<<"hello">>)),
    Crc3 = erlang:crc32(?MODULE:id(<<"hello">>)),
    Crc2 = Crc3,

    CrcC = erlang:crc32(CrcA, ?MODULE:id([<<", ">>, <<"World!">>])),
    Crc1 = CrcC,
    ok.

test_crc32_combine() ->
    Crc1 = erlang:crc32(?MODULE:id(<<"Hello">>)),
    Crc2 = erlang:crc32(?MODULE:id(<<", World!">>)),
    Combined = erlang:crc32_combine(Crc1, Crc2, byte_size(?MODULE:id(<<", World!">>))),
    Expected = erlang:crc32(?MODULE:id(<<"Hello, World!">>)),
    Expected = Combined,
    ok.

test_crc32_badarg() ->
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(42)) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id({1, 2, 3})) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(an_atom)) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(self())) end),

    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(["not", "a", "byte", -1])) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(["not", "a", "byte", 256])) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id([an_atom])) end),

    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(-1), <<"data">>) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(16#100000000), <<"data">>) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(not_an_int), <<"data">>) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id({1, 2}), <<"data">>) end),

    ok = expect_badarg(fun() -> erlang:crc32(0, ?MODULE:id(42)) end),
    ok = expect_badarg(fun() -> erlang:crc32(0, ?MODULE:id({1, 2, 3})) end),
    ok = expect_badarg(fun() -> erlang:crc32(0, ?MODULE:id([an_atom])) end),
    ok = expect_badarg(fun() -> erlang:crc32(0, ?MODULE:id(["not", "a", "byte", -1])) end),
    ok = expect_badarg(fun() -> erlang:crc32(0, ?MODULE:id(["not", "a", "byte", 256])) end),

    ok = expect_badarg(fun() -> erlang:crc32_combine(?MODULE:id(an_atom), 0, 0) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(0, ?MODULE:id(an_atom), 0) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(0, 0, ?MODULE:id(an_atom)) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(?MODULE:id(-1), 0, 0) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(0, ?MODULE:id(-1), 0) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(0, 0, ?MODULE:id(-1)) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(?MODULE:id(16#100000000), 0, 0) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(0, ?MODULE:id(16#100000000), 0) end),
    ok.

test_crc32_boundary() ->
    16#FFFFFFFF = erlang:crc32(?MODULE:id(16#FFFFFFFF), ?MODULE:id(<<>>)),
    ok.

test_crc32_empty_identity() ->
    Old = erlang:crc32(?MODULE:id(<<"abc">>)),
    Old = erlang:crc32(?MODULE:id(Old), ?MODULE:id(<<>>)),
    C1 = erlang:crc32(?MODULE:id(<<"abc">>)),
    C1 = erlang:crc32_combine(?MODULE:id(C1), erlang:crc32(?MODULE:id(<<>>)), ?MODULE:id(0)),
    ok.

test_crc32_nested_iodata() ->
    Expected = erlang:crc32(?MODULE:id(<<"Hello">>)),
    Expected = erlang:crc32(?MODULE:id([<<"He">>, [$l, $l], [<<"o">>]])),
    ok.

test_crc32_float_badarg() ->
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(1.0), <<"data">>) end),
    ok = expect_badarg(fun() -> erlang:crc32(?MODULE:id(1.5), <<"data">>) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(?MODULE:id(1.0), 0, 0) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(0, ?MODULE:id(1.5), 0) end),
    ok = expect_badarg(fun() -> erlang:crc32_combine(0, 0, ?MODULE:id(1.0)) end),
    ok.

test_crc32_cross_check() ->
    A = ?MODULE:id(<<"Hello">>),
    B = ?MODULE:id(<<", World!">>),
    erlang:crc32([A, B]) =:= erlang:crc32(erlang:crc32(A), B) orelse error(cross_check_incremental),
    erlang:crc32_combine(erlang:crc32(A), erlang:crc32(B), byte_size(B)) =:=
        erlang:crc32(<<A/binary, B/binary>>) orelse error(cross_check_combine),

    C = ?MODULE:id(<<"short">>),
    D = ?MODULE:id(<<"a somewhat longer binary for testing">>),
    erlang:crc32([C, D]) =:= erlang:crc32(erlang:crc32(C), D) orelse
        error(cross_check_incremental2),
    erlang:crc32_combine(erlang:crc32(C), erlang:crc32(D), byte_size(D)) =:=
        erlang:crc32(<<C/binary, D/binary>>) orelse error(cross_check_combine2),

    E = ?MODULE:id(<<"x">>),
    F = ?MODULE:id(<<"y">>),
    erlang:crc32([E, F]) =:= erlang:crc32(erlang:crc32(E), F) orelse
        error(cross_check_incremental3),
    erlang:crc32_combine(erlang:crc32(E), erlang:crc32(F), byte_size(F)) =:=
        erlang:crc32(<<E/binary, F/binary>>) orelse error(cross_check_combine3),
    ok.

expect_badarg(F) ->
    try
        F(),
        unexpected
    catch
        error:badarg -> ok
    end.

id(X) -> X.
