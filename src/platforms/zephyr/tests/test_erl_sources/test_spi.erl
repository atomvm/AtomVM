%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_spi).
-export([start/0]).

start() ->
    {ok, Resource} = spi:init([]),
    {ok, <<>>} = spi:transceive(Resource, 1000000, 0, <<>>),
    ok = spi:deinit(Resource),
    SPI = spi:open([
        {bus_config, []},
        {device_config, [{test_device, [{address_len_bits, 8}]}]}
    ]),
    {error, {unknown_device, missing}} = spi:write(SPI, missing, #{}),
    ok = spi:close(SPI),
    ok.
