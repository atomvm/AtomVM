%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_dac).
-export([start/0]).

%% 8-bit mid-scale and full-scale codes.
-define(MID, 128).
-define(MAX, 255).

start() ->
    {ok, DAC} = dac:open([{channel, 0}, {resolution, 8}]),
    ok = dac:write(DAC, 0),
    ok = dac:write(DAC, ?MID),
    ok = dac:write(DAC, ?MAX),
    ok = dac:close(DAC),
    ok.
