%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(test_badarith).

-export([start/0, id/1, add2/2, mul2/2, div2/2, rem2/2, abs1/1, neg1/1]).

start() ->
    add2(id(8), id(nan)) + sub2(id(8), id(nan)) + mul2(id(nan), id(-1)) + div2(id(5), id(nan)) +
        rem2(id(1), id([])) + abs1(id(nan)) + neg1(id(nan)) + div2(id(5), id(0)) * 256 +
        rem2(id(5), id(0)) * 256.

id(X) ->
    X.

add2(A, B) ->
    try A + B of
        Result -> Result
    catch
        error:badarith -> -1;
        _:_ -> -2
    end.

sub2(A, B) ->
    try A - B of
        Result -> Result
    catch
        error:badarith -> -4;
        _:_ -> -8
    end.

mul2(A, B) ->
    try A * B of
        Result -> Result
    catch
        error:badarith -> -16;
        _:_ -> -32
    end.

div2(A, B) ->
    try A div B of
        Result -> Result
    catch
        error:badarith -> -64;
        _:_ -> -128
    end.

rem2(A, B) ->
    try A rem B of
        Result -> Result
    catch
        error:badarith -> -256;
        _:_ -> -512
    end.

abs1(A) ->
    try abs(A) of
        Result -> Result
    catch
        error:badarg -> -1024;
        _:_ -> -2048
    end.

neg1(A) ->
    try -A of
        Result -> Result
    catch
        error:badarith -> -4096;
        _:_ -> -8192
    end.
