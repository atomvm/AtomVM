%
% This file is part of AtomVM.
%
% Copyright 2017 Davide Bettio <davide@uninstall.it>
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

-module(biggerdifference).

-export([start/0, willnotbecalled/1, willnotbecalled/2]).

start() ->
    diff1500(500) div 2.

diff1500(A) ->
    1500 - double(A).

double(0) ->
    0;
double(I) ->
    id(I) * 2.

id(0) ->
    0;
id(I) ->
    I.

willnotbecalled(0) ->
    2047;
willnotbecalled(I) ->
    2000 div I.

willnotbecalled(A, B) ->
    (A - B) * 2 + 2047.
