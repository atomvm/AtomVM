%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_is_builtin).

-export([start/0, id/1]).

%% AtomVM's erlang:is_builtin/3 reports whether a function is implemented
%% natively (as a BIF or NIF) *in AtomVM*, mirroring BEAM's "implemented in C"
%% semantics but against AtomVM's own registry. It therefore diverges from BEAM
%% for functions that one VM implements natively and the other implements in
%% Erlang.
%%
%% This test runs on both BEAM and AtomVM: the parity cases must agree on both
%% VMs, while known_divergences/1 asserts the *expected* per-VM answer. Running
%% it against BEAM as well keeps the documented divergences honest -- if BEAM
%% ever changes which functions it implements natively, or if AtomVM gains a
%% native implementation, the corresponding assertion here will fail and force
%% the divergence list (and the is_builtin/3 documentation in the erlang
%% module) to be revisited.
%%
%% Arguments are funneled through id/1 so the compiler cannot constant-fold the
%% calls using the host BEAM's is_builtin/3 at compile time.
start() ->
    ok = builtin_in_both(),
    ok = emulated_in_both(),
    ok = known_divergences(erlang:system_info(machine)),
    ok = not_builtin(),
    ok = bad_arguments(),
    0.

%% Functions that are native BIFs on both BEAM and AtomVM.
builtin_in_both() ->
    true = ib(erlang, abs, 1),
    true = ib(erlang, length, 1),
    true = ib(erlang, is_atom, 1),
    true = ib(erlang, byte_size, 1),
    true = ib(erlang, hd, 1),
    true = ib(erlang, tuple_size, 1),
    true = ib(erlang, '+', 2),
    true = ib(erlang, function_exported, 3),
    true = ib(erlang, is_builtin, 3),
    %% cross-module native functions
    true = ib(lists, keyfind, 3),
    ok.

%% Functions implemented in Erlang (not native) on both BEAM and AtomVM.
emulated_in_both() ->
    false = ib(lists, foldl, 3),
    false = ib(lists, map, 2),
    false = ib(lists, seq, 2),
    false = ib(proplists, get_value, 2),
    ok.

%% Functions where AtomVM and BEAM disagree because one implements them
%% natively while the other emulates them in Erlang. Each case asserts the
%% answer *both* VMs are expected to give, so the divergence is verified rather
%% than merely documented.
known_divergences(Machine) ->
    %% erlang:atom_to_binary/1: native in AtomVM, an Erlang wrapper around
    %% atom_to_binary/2 on BEAM.
    AtomToBinary1 = ib(erlang, atom_to_binary, 1),
    %% erlang:md5/1: a native C BIF on BEAM, but on AtomVM it exists only as an
    %% Erlang function delegating to crypto, so it is not a BIF.
    Md5 = ib(erlang, md5, 1),
    %% maps:get/2: a native BIF on BEAM, emulated in Erlang on AtomVM.
    MapsGet = ib(maps, get, 2),
    case Machine of
        "BEAM" ->
            false = AtomToBinary1,
            true = Md5,
            true = MapsGet,
            ok;
        _ ->
            true = AtomToBinary1,
            false = Md5,
            false = MapsGet,
            ok
    end.

%% Names that are not native functions anywhere.
not_builtin() ->
    %% bitsize/1 is not a real function; only bit_size/1 exists.
    false = ib(erlang, bitsize, 1),
    %% wrong arity for an existing BIF
    false = ib(erlang, abs, 2),
    %% negative arity never matches
    false = ib(erlang, abs, -1),
    %% unknown module and unknown function
    false = ib(no_such_module, no_such_function, 0),
    false = ib(erlang, definitely_not_a_bif, 0),
    ok.

%% Type errors raise badarg, matching BEAM.
bad_arguments() ->
    ok = expect_badarg(fun() -> ib(123, foo, 0) end),
    ok = expect_badarg(fun() -> ib(erlang, 123, 0) end),
    ok = expect_badarg(fun() -> ib(erlang, foo, not_an_integer) end),
    ok.

ib(Module, Function, Arity) ->
    erlang:is_builtin(?MODULE:id(Module), ?MODULE:id(Function), ?MODULE:id(Arity)).

expect_badarg(Fun) ->
    try Fun() of
        Ret -> {unexpected, Ret}
    catch
        error:badarg -> ok;
        C:E -> {unexpected, C, E}
    end.

id(X) ->
    X.
