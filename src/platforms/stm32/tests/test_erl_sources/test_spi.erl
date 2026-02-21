%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
    {ok, {_Baudrate, SPI}} = spi:init(1, 1000000),
    1 = spi:transmit(SPI, <<16#9F>>, 1000),
    {ok, RxData} = spi:receive_(SPI, 3, 1000),
    3 = byte_size(RxData),
    {ok, TxRxData} = spi:transmit_receive(SPI, <<16#9F, 0, 0, 0>>, 1000),
    4 = byte_size(TxRxData),
    {ok, ActualBaud} = spi:set_baudrate(SPI, 500000),
    true = is_integer(ActualBaud) andalso ActualBaud > 0,
    ok = spi:set_format(SPI, 8, 0, 0),
    ready = spi:get_state(SPI),
    0 = spi:get_error(SPI),
    ok = spi:abort(SPI),
    ok = spi:deinit(SPI),
    ok = spi:deinit(SPI),
    erlang:display(spi_done).
