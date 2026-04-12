%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_uart).

-export([start/0]).

start() ->
    erlang:display(test_uart_start),
    % Initialize USART2: PA2 (TX), PA3 (RX), AF7, 115200 baud
    % All test platforms use USART2 on PA2/PA3 with AF7 (AF is not checked by Renode)
    {ok, UART} = uart:init([
        {peripheral, 2},
        {tx, {a, 2}},
        {rx, {a, 3}},
        {af, 7},
        {speed, 115200}
    ]),
    % Test TX: write "hello" and verify byte count
    5 = uart:write(UART, <<"hello">>, 5000),
    ready = uart:get_state(UART),
    0 = uart:get_error(UART),
    % Signal the Renode test that we are ready to receive.
    % The robot will inject 5 bytes into USART2 upon seeing this.
    erlang:display(uart_waiting),
    % Test RX: read back the 5 bytes injected by Renode
    {ok, <<"hello">>} = uart:read(UART, 5, 5000),
    ok = uart:deinit(UART),
    erlang:display(uart_done).
