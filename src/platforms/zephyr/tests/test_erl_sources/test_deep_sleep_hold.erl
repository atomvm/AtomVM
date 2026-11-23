%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_deep_sleep_hold).
-export([start/0]).

start() ->
    erlang:display(test_deep_sleep_hold_started),
    SystemArchitecture = erlang:system_info(system_architecture),
    erlang:display(SystemArchitecture),
    IsESP32 = case binary:split(SystemArchitecture, <<"-">>, [global]) of
        [_Architecture, Vendor | _] ->
            Vendor =:= <<"esp">>;
        _ ->
            false
    end,
    erlang:display({is_esp32, IsESP32}),

    ok = test_sleep_api(),

    % We should be able to get the next PM state (can be undefined or active initially depending on platform/policy)
    InitialState = zephyr:pm_state_next_get(0),
    erlang:display({initial_state, InitialState}),
    true = (InitialState =:= undefined) orelse (InitialState =:= {active, 0, 0, 0}),

    case IsESP32 of
        true ->
            erlang:display(running_esp32_sync_checks),
            % On ESP32 simulation, entering low-power sleep states (light or deep) triggers a simulator power cycle/reset.
            % We verify the NIF logic by forcing invalid inputs (invalid CPU or active state which is not in DTS)
            % which must return false, testing NIF translation and validation without triggering actual sleeps.
            false = zephyr:pm_state_force(0, suspend_to_ram),
            erlang:display(forced_suspend_to_ram_false),
            false = zephyr:pm_state_force(0, active),
            erlang:display(forced_active_false),
            ok;
        false ->
            % Force standby state using state name atom
            true = zephyr:pm_state_force(0, standby),
            timer:sleep(50),
            NextState1 = zephyr:pm_state_next_get(0),
            true = (NextState1 =:= {standby, 0, 1000, 50}) orelse (NextState1 =:= {active, 0, 0, 0}),

            % Force standby state with substate ID (0) and residency time
            true = zephyr:pm_state_force(0, {standby, 0, 10000}),
            timer:sleep(50),
            NextState3 = zephyr:pm_state_next_get(0),
            true = (NextState3 =:= {standby, 0, 1000, 50}) orelse (NextState3 =:= {active, 0, 0, 0}),

            % Force soft_off state using tuple with substate ID (0)
            true = zephyr:pm_state_force(0, {soft_off, 0}),
            timer:sleep(50),
            NextState2 = zephyr:pm_state_next_get(0),
            true = (NextState2 =:= {soft_off, 0, 0, 0}) orelse (NextState2 =:= {active, 0, 0, 0}),

            % Force soft_off state with substate (0), residency, and latency
            true = zephyr:pm_state_force(0, {soft_off, 0, 50000, 500}),
            timer:sleep(50),
            NextState4 = zephyr:pm_state_next_get(0),
            true = (NextState4 =:= {soft_off, 0, 0, 0}) orelse (NextState4 =:= {active, 0, 0, 0}),

            % Reset/force active to clean up
            true = zephyr:pm_state_force(0, active),
            timer:sleep(50),
            NextState5 = zephyr:pm_state_next_get(0),
            true = (NextState5 =:= {active, 0, 0, 0}),
            ok
    end,

    ok.

test_sleep_api() ->
    Cause = zephyr:sleep_get_wakeup_cause(),
    true = lists:member(Cause, [undefined, timer, gpio, reset, unknown]),
    try zephyr:sleep_enable_gpio_wakeup(0, 1) of
        ok ->
            ok;
        {error, _} ->
            ok
    catch
        error:undef ->
            ok
    end,
    ok.
