%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(test_utf8_atoms).
-export([start/0, conv/3, get_atom/1, get_list/1, get_binary/1, conv2/3]).

start() ->
    32767 - test_from_atom() +
        4095 - test_to_atom() +
        63 - test_missing_atom() +
        7 - test_latin1_convs() +
        7 - test_invalid_bins().

test_latin1_convs() ->
    comp(?MODULE:conv2(binary, l1, ?MODULE:get_binary(l1s)), 'ÂµÃ\230Ã¥') +
        comp(?MODULE:conv2(binary, l1, ?MODULE:get_binary(l1_mixed)), get_atom(l1_mixed)) * 2 +
        comp(?MODULE:conv(binary, l1, ?MODULE:get_atom(l1_mixed)), get_binary(l1_mixed)) * 4.

test_from_atom() ->
    test_to_list(l1) +
        test_to_list(l1s) * 2 +
        test_to_list(gr) * 4 +
        test_to_list(jp) * 8 +
        test_to_list(jp_mixed) * 16 +
        test_to_l1bin(l1) * 32 +
        test_to_l1bin(l1s_plain) * 64 +
        test_to_l1bincatch(gr) * 128 +
        test_to_l1bincatch(jp) * 256 +
        test_to_l1bincatch(jp_mixed) * 512 +
        test_to_u8bin(l1) * 1024 +
        test_to_u8bin(l1s) * 2048 +
        test_to_u8bin(gr) * 4096 +
        test_to_u8bin(jp) * 8192 +
        test_to_u8bin(jp_mixed) * 16384.

test_to_atom() ->
    test_from_list(l1) +
        test_from_list(l1s) * 2 +
        test_from_list(gr) * 4 +
        test_from_list(jp) * 8 +
        test_from_list(jp_mixed) * 16 +
        test_from_l1bin(l1) * 32 +
        test_from_l1bin(l1s_plain) * 64 +
        test_from_u8bin(l1) * 128 +
        test_from_u8bin(l1s) * 256 +
        test_from_u8bin(gr) * 512 +
        test_from_u8bin(jp) * 1024 +
        test_from_u8bin(jp_mixed) * 2048.

test_missing_atom() ->
    comp(
        erlang:list_to_atom(get_list(l1s_missing)),
        erlang:binary_to_atom(get_binary(l1s_missing), utf8)
    ) +
        comp(
            erlang:list_to_atom(get_list(jp_mixed_missing)),
            erlang:binary_to_atom(get_binary(jp_mixed_missing), utf8)
        ) * 2 +
        comp(
            erlang:list_to_atom(get_list(l1s_missing)),
            erlang:binary_to_atom(get_binary(l1s_missing), unicode)
        ) * 4 +
        comp(
            erlang:list_to_atom(get_list(jp_mixed_missing)),
            erlang:binary_to_atom(get_binary(jp_mixed_missing), unicode)
        ) * 8 +
        comp_opt(
            fun() -> erlang:list_to_atom(get_list(l1s_missing)) end,
            fun() -> erlang:binary_to_atom(get_binary(l1s_missing)) end
        ) * 16 +
        comp_opt(
            fun() -> erlang:list_to_atom(get_list(jp_mixed_missing)) end,
            fun() -> erlang:binary_to_atom(get_binary(jp_mixed_missing)) end
        ) * 32.

test_invalid_bins() ->
    test_from_u8bincatch(invalid1) +
        test_from_u8bincatch(invalid2) * 2 +
        test_from_u8bincatch(invalid2) * 4.

test_to_list(Id) ->
    case ?MODULE:conv(list, x, ?MODULE:get_atom(Id)) == ?MODULE:get_list(Id) of
        true ->
            1;
        false ->
            erlang:display({list, Id}),
            0
    end.

test_to_l1bin(Id) ->
    case ?MODULE:conv(binary, l1, ?MODULE:get_atom(Id)) == ?MODULE:get_binary(Id) of
        true ->
            1;
        false ->
            erlang:display({l1bin, Id}),
            0
    end.

test_to_l1bincatch(Id) ->
    try ?MODULE:conv(binary, l1, ?MODULE:get_atom(Id)) of
        _X ->
            erlang:display({err, Id}),
            0
    catch
        error:badarg ->
            1
    end.

test_to_u8bin(Id) ->
    case ?MODULE:conv(binary, u8, ?MODULE:get_atom(Id)) == ?MODULE:get_binary(Id) of
        true ->
            1;
        false ->
            erlang:display({u8bin, Id}),
            0
    end.

test_from_list(Id) ->
    case ?MODULE:conv2(list, x, ?MODULE:get_list(Id)) == ?MODULE:get_atom(Id) of
        true ->
            1;
        false ->
            erlang:display({flist, Id}),
            0
    end.

test_from_l1bin(Id) ->
    case ?MODULE:conv2(binary, l1, ?MODULE:get_binary(Id)) == ?MODULE:get_atom(Id) of
        true ->
            1;
        false ->
            erlang:display({fl1bin, Id}),
            0
    end.

test_from_u8bin(Id) ->
    case ?MODULE:conv2(binary, u8, ?MODULE:get_binary(Id)) == ?MODULE:get_atom(Id) of
        true ->
            1;
        false ->
            erlang:display({fu8bin, Id}),
            0
    end.

test_from_u8bincatch(Id) ->
    try ?MODULE:conv2(binary, u8, ?MODULE:get_binary(Id)) of
        _X ->
            erlang:display({u8err, Id}),
            0
    catch
        error:badarg ->
            1
    end.

conv(list, _Fmt, Atom) ->
    erlang:atom_to_list(Atom);
conv(binary, l1, Atom) ->
    erlang:atom_to_binary(Atom, latin1);
conv(binary, u8, Atom) ->
    erlang:atom_to_binary(Atom, utf8).

conv2(list, _Fmt, S) ->
    erlang:list_to_atom(S);
conv2(binary, l1, S) ->
    erlang:binary_to_atom(S, latin1);
conv2(binary, u8, S) ->
    erlang:binary_to_atom(S, utf8).

comp(A, A) -> 1;
comp(_A, _B) -> 0.

comp_opt(Fun1, Fun2) ->
    case erlang:system_info(machine) of
        "BEAM" ->
            case erlang:system_info(otp_release) of
                Version when Version >= "23" -> comp(Fun1(), Fun2());
                _OldVersion -> 1
            end;
        _ ->
            comp(Fun1(), Fun2())
    end.

get_atom(Id) ->
    case Id of
        l1 -> 'abcd';
        l1_mixed -> 'testé';
        l1s -> 'µØå';
        l1s_plain -> 'µØå';
        gr -> 'ΓΔ';
        jp -> 'アーラン';
        jp_mixed -> 'latin1じゃない'
    end.

get_list(Id) ->
    case Id of
        l1 -> "abcd";
        l1s -> "µØå";
        l1s_missing -> "µ_å";
        gr -> "ΓΔ";
        jp -> "アーラン";
        jp_mixed -> "latin1じゃない";
        jp_mixed_missing -> "latin1_じゃない"
    end.

get_binary(Id) ->
    case Id of
        l1 -> <<"abcd"/utf8>>;
        l1_mixed -> <<"testé">>;
        l1s_plain -> <<"µØå">>;
        l1s -> <<"µØå"/utf8>>;
        l1s_missing -> <<"µ_å"/utf8>>;
        gr -> <<"ΓΔ"/utf8>>;
        jp -> <<"アーラン"/utf8>>;
        jp_mixed -> <<"latin1じゃない"/utf8>>;
        jp_mixed_missing -> <<"latin1_じゃない"/utf8>>;
        invalid1 -> <<230>>;
        invalid2 -> <<16#f0, 16#90, 16#28, 16#bc>>;
        invalid3 -> <<16#fc, 16#a1, 16#a1, 16#a1, 16#a1, 16#a1>>
    end.
