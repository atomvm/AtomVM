%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_platform).
-export([start/0]).

start() ->
    ok = test_timer_get_time(),
    ok = test_reset_reason(),
    ok = test_mac(),
    ok = test_chip_info(),
    ok.

test_timer_get_time() ->
    T0 = zephyr:timer_get_time(),
    true = is_integer(T0) andalso T0 >= 0,
    receive
    after 20 -> ok
    end,
    T1 = zephyr:timer_get_time(),
    true = T1 >= T0,
    ok.

test_reset_reason() ->
    try zephyr:reset_reason() of
        undefined ->
            ok;
        Reasons when is_list(Reasons) ->
            true = lists:all(fun is_atom/1, Reasons),
            ok
    catch
        error:undef ->
            ok
    end.

test_mac() ->
    try zephyr:get_default_mac() of
        {ok, Mac} ->
            true = is_binary(Mac) andalso byte_size(Mac) >= 6,
            Default = zephyr:get_mac(default),
            true = is_binary(Default) andalso byte_size(Default) >= 6,
            ok;
        {error, _} ->
            ok
    catch
        error:undef ->
            ok
    end.

test_chip_info() ->
    ChipInfo = erlang:system_info(esp32_chip_info),
    Architecture = erlang:system_info(system_architecture),
    IsEsp =
        case binary:split(Architecture, <<"-">>, [global]) of
            [_Arch, <<"esp">>, <<"zephyr">>] ->
                true;
            _ ->
                false
        end,
    case {IsEsp, ChipInfo} of
        {false, undefined} ->
            ok;
        {true, #{model := Model, cores := Cores, features := Features, revision := Revision}} ->
            true = lists:member(
                Model,
                [esp32, esp32_s2, esp32_s3, esp32_c2, esp32_c3, esp32_c5, esp32_c6, esp32_h2]
            ),
            true = is_integer(Cores) andalso Cores >= 1,
            true = is_list(Features),
            true = lists:all(fun is_atom/1, Features),
            true = is_integer(Revision) andalso Revision >= 0,
            ok
    end.
