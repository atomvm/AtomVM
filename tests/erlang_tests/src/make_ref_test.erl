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

-module(make_ref_test).

-export([start/0, compare/2, do_make_ref/0, to_tuple/1]).

start() ->
    ARef = do_make_ref(),
    ARefT = to_tuple(ARef),
    compare(ARefT, ARef) + compare(to_tuple(make_ref()), ARef) * 32.

do_make_ref() ->
    make_ref().

to_tuple(Something) ->
    {Something}.

compare(Something1, {Something2}) when Something1 == Something2 ->
    1;
compare({Something1}, Something2) when Something1 == Something2 ->
    2;
compare(_Any1, _Any2) ->
    4.
