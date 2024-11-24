%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(fconv_fail_invalid).

-export([start/0, deg_min_nsew_to_decimal/1]).

start() ->
    try ?MODULE:deg_min_nsew_to_decimal({5, nil, e}) of
        _Any -> -1
    catch
        error:badarith -> 0
    end.

deg_min_nsew_to_decimal(Coord) ->
    {Deg, Min, Nsew} = Coord,
    DecimalCoord = Deg + Min / 60,
    case Nsew of
        n -> DecimalCoord;
        s -> -DecimalCoord;
        e -> DecimalCoord;
        w -> -DecimalCoord
    end.
