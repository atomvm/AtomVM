%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc Portable DAC interface for Zephyr.
%%
%% Controllers are selected by the `atomvm,dac' chosen node, by integer index
%% (`dac0'/`dac' as 0, `dac1' as 1), or by device name. Channel numbers and
%% resolution follow the board's DAC binding. Classic ESP32 channel 0 is
%% GPIO25 and channel 1 is GPIO26; ESP32-C3 and ESP32-S3 have no DAC.
%%
%% `write/2' takes a raw DAC code in `0 .. (1 bsl Resolution) - 1'.
-module(dac).

-export([
    open/0,
    open/1,
    close/1,
    write/2,
    init/1,
    deinit/1
]).

-type dac() :: reference().
-type channel() :: non_neg_integer().
-type resolution() :: 1..32.
-type raw() :: non_neg_integer().
-export_type([dac/0, channel/0, resolution/0, raw/0]).

-spec open() -> {ok, dac()} | {error, term()}.
open() ->
    open([]).

-spec open(Options :: list()) -> {ok, dac()} | {error, term()}.
open(Options) ->
    ?MODULE:init(Options).

-spec close(DAC :: dac()) -> ok.
close(DAC) ->
    ?MODULE:deinit(DAC).

%% @doc Write a raw DAC code for the channel configured at `open/1'.
-spec write(DAC :: dac(), Value :: raw()) -> ok | {error, term()}.
write(_DAC, _Value) ->
    erlang:nif_error(undefined).

-spec init(Options :: list()) -> {ok, dac()} | {error, term()}.
init(_Options) ->
    erlang:nif_error(undefined).

-spec deinit(DAC :: dac()) -> ok.
deinit(_DAC) ->
    erlang:nif_error(undefined).
