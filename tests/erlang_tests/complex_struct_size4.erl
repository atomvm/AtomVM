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

-module(complex_struct_size4).
-export([start/0, a/0, b/0, c/0, ac/0, d/0, e/0, de/0, f/0, g/0, h/0, i/0, j/0, k/0, l/0, kl/0, m/0, n/0, o/0, p/0, q/0, r/0, s/0, t/0, z/0]).

start() ->
    erts_debug:flat_size(z()).

a() ->
    {'+', 1, 3}. % 4

b() ->
    {'div', 5, 3}. % 1

c() ->
    {'-', 4, b()}. % 3

ac() ->
    {'*', a(), c()}. %12

d() ->
    {'-', ac(), 13}. % -1

e() ->
    {'!', 3}. % 6

de() ->
    {'*', d(), e()}. % -6

f() ->
    {'*', 2, 2}. % 4

g() ->
    {'+', f(), h()}. % 12

h() ->
    {'*', 4, 2}. % 8

i() ->
    {'+', de(), g()}. % 6

j() ->
    {'.', [1, 2, 3], [10, 20, 30]}. % 140

k() ->
    {'-', j(), i()}. % 134

l() ->
    {'+', [0, 7, 8]}. % 15

kl() ->
    {'-', k(), l()}. % 119

m() ->
    {'-', kl()}. % - 119

n() ->
    {'-', m()}. % 119

o() ->
    {'-', n()}. % - 119

p() ->
    {'+', 3, 2}. % 5

q() ->
    {'+', 15, r()}. % 20

r() ->
    {'+', 1, 4}. % 5

s() ->
    {'*', p(), q()}. % 100

t() ->
    {'+', o(), s()}. % -19

u() ->
    {'+', 4, 1}. % 5

v() ->
    {'!', u()}. % 120

w() ->
    {'-', v(), 119}. % 1

x() ->
    {'+', w(), t()}. % - 18

y() ->
    {'-', x()}. % 18

z() ->
    {'-', y()}. % -18
