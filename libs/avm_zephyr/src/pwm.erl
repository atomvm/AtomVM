%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc Portable PWM interface for Zephyr.
%%
%% Controllers are selected by the `atomvm,pwm' chosen node, by integer index
%% (`pwm0'/`ledc0' as 0, `pwm1' as 1, ...), or by device name. Channel numbers
%% and pin routing follow the board's devicetree PWM bindings.
%%
%% Period and pulse widths for `set/4,5' are nanoseconds. Use `set_cycles/4,5'
%% for hardware cycle units when preferred.
-module(pwm).

-export([
    open/0,
    open/1,
    close/1,
    set/4,
    set/5,
    set_cycles/4,
    set_cycles/5,
    get_cycles_per_sec/2,
    init/1,
    deinit/1
]).

-type pwm() :: reference().
-type channel() :: non_neg_integer().
-type nanoseconds() :: non_neg_integer().
-type cycles() :: non_neg_integer().
-type polarity() :: normal | inverted.
-type flags() :: polarity() | [polarity()].
-export_type([pwm/0, channel/0, nanoseconds/0, cycles/0, polarity/0, flags/0]).

-spec open() -> {ok, pwm()} | {error, term()}.
open() ->
    open([]).

-spec open(Options :: list()) -> {ok, pwm()} | {error, term()}.
open(Options) ->
    ?MODULE:init(Options).

-spec close(PWM :: pwm()) -> ok.
close(PWM) ->
    ?MODULE:deinit(PWM).

%% @doc Set period and pulse width in nanoseconds.
-spec set(PWM :: pwm(), Channel :: channel(), PeriodNs :: nanoseconds(), PulseNs :: nanoseconds()) ->
    ok | {error, term()}.
set(PWM, Channel, PeriodNs, PulseNs) ->
    ?MODULE:set(PWM, Channel, PeriodNs, PulseNs, normal).

-spec set(
    PWM :: pwm(),
    Channel :: channel(),
    PeriodNs :: nanoseconds(),
    PulseNs :: nanoseconds(),
    Flags :: flags()
) -> ok | {error, term()}.
set(_PWM, _Channel, _PeriodNs, _PulseNs, _Flags) ->
    erlang:nif_error(undefined).

%% @doc Set period and pulse width in hardware cycles.
-spec set_cycles(
    PWM :: pwm(),
    Channel :: channel(),
    PeriodCycles :: cycles(),
    PulseCycles :: cycles()
) -> ok | {error, term()}.
set_cycles(PWM, Channel, PeriodCycles, PulseCycles) ->
    ?MODULE:set_cycles(PWM, Channel, PeriodCycles, PulseCycles, normal).

-spec set_cycles(
    PWM :: pwm(),
    Channel :: channel(),
    PeriodCycles :: cycles(),
    PulseCycles :: cycles(),
    Flags :: flags()
) -> ok | {error, term()}.
set_cycles(_PWM, _Channel, _PeriodCycles, _PulseCycles, _Flags) ->
    erlang:nif_error(undefined).

-spec get_cycles_per_sec(PWM :: pwm(), Channel :: channel()) ->
    {ok, non_neg_integer()} | {error, term()}.
get_cycles_per_sec(_PWM, _Channel) ->
    erlang:nif_error(undefined).

-spec init(Options :: list()) -> {ok, pwm()} | {error, term()}.
init(_Options) ->
    erlang:nif_error(undefined).

-spec deinit(PWM :: pwm()) -> ok.
deinit(_PWM) ->
    erlang:nif_error(undefined).
