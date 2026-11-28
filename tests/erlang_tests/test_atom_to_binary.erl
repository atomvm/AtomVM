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

-module(test_atom_to_binary).

-export([start/0, f/1, h/1, compare_bin/2, id/1]).

start() ->
    ok = test_latin1_and_utf8_encodings(),
    compare_bin(f(hello_world), <<"hello_world">>) - h(15) - i(test_bin).

f(A) when is_binary(A) ->
    binaries_not_ok;
f(A) ->
    atom_to_binary(A, latin1).

g(A) ->
    atom_to_binary(A, not_good).

compare_bin(Bin1, Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1).

compare_bin(_Bin1, _Bin2, -1) ->
    1;
compare_bin(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    case binary:at(Bin2, Index) of
        B1 ->
            compare_bin(Bin1, Bin2, Index - 1);
        _Any ->
            0
    end.

h(A) ->
    try f(A) of
        _AnyVal -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1024
    end.

i(A) ->
    try g(A) of
        _AnyVal -> 1
    catch
        error:badarg -> 0;
        _:_ -> -1024
    end.

test_latin1_and_utf8_encodings() ->
    [
        begin
            Expected = duplicate_bin(<<16#E9>>, N, <<>>),
            Atom = list_to_atom(id(binary_to_list(Expected))),
            Keep = {keep, N, Atom, Expected},
            true = erlang:garbage_collect(),
            Bin = atom_to_binary(id(Atom), latin1),
            true = erlang:garbage_collect(),
            N = byte_size(Bin),
            true = Bin =:= Expected,
            true = Keep =:= {keep, N, Atom, Expected}
        end
     || N <- [32, 40, 63, 64]
    ],
    ok = assert_badarg(fun() -> atom_to_binary(list_to_atom(id([16#E9, 16#100])), latin1) end),
    ok = assert_badarg(fun() -> atom_to_binary(list_to_atom(id([16#20AC])), latin1) end),
    ok = assert_badarg(fun() -> atom_to_binary(list_to_atom(id([16#1F600])), latin1) end),
    B2 = atom_to_binary(list_to_atom(id([16#E9, 16#20AC, 16#1F600])), unicode),
    B2 = atom_to_binary(list_to_atom(id([16#E9, 16#20AC, 16#1F600])), utf8),
    <<16#C3, 16#A9, 16#E2, 16#82, 16#AC, 16#F0, 16#9F, 16#98, 16#80>> = B2,
    ok.

assert_badarg(F) ->
    try
        R = F(),
        {should_have_raised, R}
    catch
        error:badarg -> ok
    end.

id(X) ->
    X.

duplicate_bin(_Bin, 0, Acc) -> Acc;
duplicate_bin(Bin, N, Acc) -> duplicate_bin(Bin, N - 1, <<Bin/binary, Acc/binary>>).
