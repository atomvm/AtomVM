%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc Zephyr analog-to-digital converter API.
%%
%% The default ADC controller and channel are selected through the first
%% `io-channels' entry on the devicetree `zephyr,user' node. Resolution, gain,
%% reference, and acquisition time are likewise configured in devicetree.
-module(adc).

-export([open/0, open/1, close/1, read/1, read/2, init/1, deinit/1]).

-type adc() :: reference().
-type reading() :: {Raw :: integer(), Millivolts :: integer() | undefined}.
-export_type([adc/0, reading/0]).

-spec open() -> {ok, adc()} | {error, term()}.
open() ->
    open([]).

-spec open(Options :: list()) -> {ok, adc()} | {error, term()}.
open(Options) ->
    ?MODULE:init(Options).

-spec close(ADC :: adc()) -> ok.
close(ADC) ->
    ?MODULE:deinit(ADC).

-spec read(ADC :: adc()) -> {ok, reading()} | {error, term()}.
read(ADC) ->
    ?MODULE:read(ADC, 1).

-spec read(ADC :: adc(), Samples :: 1..1024) -> {ok, reading()} | {error, term()}.
read(_ADC, _Samples) ->
    erlang:nif_error(undefined).

-spec init(Options :: list()) -> {ok, adc()} | {error, term()}.
init(_Options) ->
    erlang:nif_error(undefined).

-spec deinit(ADC :: adc()) -> ok.
deinit(_ADC) ->
    erlang:nif_error(undefined).
