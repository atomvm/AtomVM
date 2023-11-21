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

-module(guards1).

-export([start/0, comp/2]).

start() ->
    comp(3, 3) - comp(1, 2) * 2 + comp(3, 1).

comp(A, B) when A > B ->
    1;
comp(A, B) when A < B ->
    20;
comp(A, B) when A >= B ->
    300;
comp(A, B) when A =< B ->
    1000.
