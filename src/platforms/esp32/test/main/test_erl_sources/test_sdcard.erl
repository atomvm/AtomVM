%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_sdcard).
-export([start/0]).

start() ->
    {ok, SDCard} = esp:sdcard_open("sdmmc", []),
    ok = test_info(SDCard),
    ok = test_read(SDCard),
    ok = test_write_roundtrip(SDCard),
    ok = test_bad_args(SDCard),
    ok = esp:sdcard_close(SDCard),
    ok = test_use_after_close(SDCard),
    ok = test_open_bad_opts(),
    ok = test_reopen(),
    ok.

test_info(SDCard) ->
    {ok, #{sector_size := SectorSize, sector_count := SectorCount}} = esp:sdcard_info(SDCard),
    true = is_integer(SectorSize) andalso SectorSize > 0,
    true = is_integer(SectorCount) andalso SectorCount > 0,
    ok.

test_read(SDCard) ->
    {ok, #{sector_size := SectorSize}} = esp:sdcard_info(SDCard),
    {ok, Sector0} = esp:sdcard_read(SDCard, 0),
    SectorSize = byte_size(Sector0),
    ok.

test_write_roundtrip(SDCard) ->
    {ok, #{sector_size := SectorSize, sector_count := SectorCount}} = esp:sdcard_info(SDCard),
    LastSector = SectorCount - 1,
    {ok, Original} = esp:sdcard_read(SDCard, LastSector),
    Pattern = make_pattern(SectorSize),
    try
        ok = esp:sdcard_write(SDCard, LastSector, Pattern),
        {ok, Pattern} = esp:sdcard_read(SDCard, LastSector)
    after
        restore_sector(SDCard, LastSector, Original)
    end,
    {ok, Original} = esp:sdcard_read(SDCard, LastSector),
    ok.

restore_sector(SDCard, Sector, Data) ->
    try
        esp:sdcard_write(SDCard, Sector, Data)
    catch
        _:_ -> ok
    end.

test_bad_args(SDCard) ->
    {ok, #{sector_size := SectorSize, sector_count := SectorCount}} = esp:sdcard_info(SDCard),
    ok = expect_badarg(fun() -> esp:sdcard_write(SDCard, 0, <<0>>) end),
    ok = expect_badarg(fun() -> esp:sdcard_read(SDCard, SectorCount) end),
    ok = expect_badarg(fun() -> esp:sdcard_write(SDCard, SectorCount, make_pattern(SectorSize)) end),
    ok = expect_badarg(fun() -> esp:sdcard_read(SDCard, -1) end),
    ok = expect_badarg(fun() -> esp:sdcard_read(SDCard, 1 bsl 32) end),
    ok = expect_badarg(fun() -> esp:sdcard_read(SDCard, 1 bsl 64) end),
    ok = expect_badarg(fun() -> esp:sdcard_write(SDCard, 1 bsl 64, make_pattern(SectorSize)) end),
    ok.

test_use_after_close(SDCard) ->
    ok = expect_badarg(fun() -> esp:sdcard_read(SDCard, 0) end),
    ok.

test_open_bad_opts() ->
    %% sdspi without the required spi_host/cs options.
    ok = expect_badarg(fun() -> esp:sdcard_open("sdspi", []) end),
    ok.

%% Close must fully release the host so that a new open works.
test_reopen() ->
    {ok, SDCard} = esp:sdcard_open("sdmmc", []),
    {ok, _Sector0} = esp:sdcard_read(SDCard, 0),
    ok = esp:sdcard_close(SDCard),
    ok.

make_pattern(SectorSize) ->
    list_to_binary([N rem 256 || N <- lists:seq(0, SectorSize - 1)]).

expect_badarg(Fun) ->
    try Fun() of
        _ -> error
    catch
        error:badarg -> ok;
        _:_ -> not_badarg
    end.
