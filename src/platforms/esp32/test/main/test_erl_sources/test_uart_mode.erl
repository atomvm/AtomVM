%
% This file is part of AtomVM.
%
% Copyright 2026 Gabriel Mancini <gabriel.mancini@gmail.com>
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

-module(test_uart_mode).

-export([start/0]).

%% Opens and closes UART1 with each mode option; nothing is sent and no pin is
%% routed, so this needs only UART1 free (not the console).
start() ->
    ok = open_close([]),
    ok = open_close([{mode, uart}]),
    ok = open_close([{mode, rs485_half_duplex}]),
    lists:foreach(
        fun(Mode) -> ok = expect_badarg([{mode, Mode}]) end,
        [undefined, invalid_mode, 0, <<"uart">>, "rs485_half_duplex"]
    ),
    % Fails after the driver is installed: the next open checks the cleanup.
    ok = expect_badarg([{mode, rs485_half_duplex}, {flow_control, hardware}]),
    ok = open_close([{mode, rs485_half_duplex}]),
    ok.

open_close(Opts) ->
    Port = uart:open("UART1", Opts),
    true = is_port(Port),
    ok = uart:close(Port).

expect_badarg(Opts) ->
    try uart:open("UART1", Opts) of
        Port ->
            ok = uart:close(Port),
            {expected_badarg, Opts}
    catch
        error:badarg -> ok
    end.
