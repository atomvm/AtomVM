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

-module(test_badarith3).

-export([start/0, id/1, bor2/2]).

start() ->
    bor2(id(8), id(nan)) + band2(id(0), id(nan)) + bxor2(id(10000), id(nan)) +
        bsl2(id(1), id(nan)) + bsr2(id(1), id(nan)) + bnot1(id(nan)).

id(X) ->
    X.

bor2(A, B) ->
    try A bor B of
        Result -> Result
    catch
        error:badarith -> -1;
        _:_ -> -2
    end.

band2(A, B) ->
    try A band B of
        Result -> Result
    catch
        error:badarith -> -4;
        _:_ -> -8
    end.

bxor2(A, B) ->
    try A bxor B of
        Result -> Result
    catch
        error:badarith -> -16;
        _:_ -> -32
    end.

bsl2(A, B) ->
    try A bsl B of
        Result -> Result
    catch
        error:badarith -> -64;
        _:_ -> -128
    end.

bsr2(A, B) ->
    try A bsr B of
        Result -> Result
    catch
        error:badarith -> -256;
        _:_ -> -512
    end.

bnot1(A) ->
    try bnot A of
        Result -> Result
    catch
        error:badarith -> -1024;
        _:_ -> -2048
    end.
