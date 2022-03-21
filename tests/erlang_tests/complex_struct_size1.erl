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

-module(complex_struct_size1).

-export([start/0, abcdefg/0]).

start() ->
    erts_debug:flat_size(abcdefg()).

a() ->
    {a, 0}.

b() ->
    {b, 1}.

c() ->
    {c, 2}.

d() ->
    {d, 3}.

e() ->
    {e, 4}.

f() ->
    {f, 5}.

g() ->
    {g, 6}.

ab() ->
    {a(), b()}.

cde() ->
    {c(), d(), e()}.

fg() ->
    {f(), g()}.

cdefg() ->
    {cde(), fg()}.

abcdefg() ->
    {ab(), cdefg()}.
