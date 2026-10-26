%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
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

-module(test_spi).
-export([start/0]).

start() ->
    ok = open_write_close(),
    % Reopening the same peripheral also checks that close released the bus.
    open_write_close().

open_write_close() ->
    SPI = spi:open(#{
        bus_config => #{mosi => 5, sclk => 4, peripheral => "spi2"},
        device_config => #{
            test_device => #{
                clock_speed_hz => 2500000, mode => 0, cs => -1, address_len_bits => 0
            }
        }
    }),
    MonitorRef = monitor(port, SPI),
    % No slave or loopback wiring is needed for a write-only transaction.
    ok = spi:write(SPI, test_device, #{write_data => <<0>>}),
    ok = spi:close(SPI),
    % The reply alone is not enough: the double free happens during termination.
    receive
        {'DOWN', MonitorRef, port, SPI, normal} -> ok
    after 5000 ->
        timeout
    end.
