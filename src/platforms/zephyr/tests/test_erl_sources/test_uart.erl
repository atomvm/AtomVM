%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_uart).
-export([start/0]).

start() ->
    ok = test_low_level_api(),
    ok = test_high_level_api(),
    ok.

test_low_level_api() ->
    {ok, Resource} = uart:init([]),
    ready = uart:get_state(Resource),
    0 = uart:get_error(Resource),
    0 = uart:write(Resource, <<>>, 0),
    {ok, <<>>} = uart:read(Resource, 0, 0),
    ok = uart:abort(Resource),
    ok = uart:deinit(Resource),
    ok.

test_high_level_api() ->
    UART = uart:open([]),
    ok = uart:write(UART, []),
    {error, timeout} = uart:read(UART),
    ok = uart:close(UART),
    ok.
