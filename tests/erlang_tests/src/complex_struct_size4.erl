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

-export([
    start/0,
    a/0,
    b/0,
    c/0,
    ac/0,
    d/0,
    e/0,
    de/0,
    f/0,
    g/0,
    h/0,
    i/0,
    j/0,
    k/0,
    l/0,
    kl/0,
    m/0,
    n/0,
    o/0,
    p/0,
    q/0,
    r/0,
    s/0,
    t/0,
    z/0
]).

start() ->
    erts_debug:flat_size(z()).

a() ->
    % 4
    {'+', 1, 3}.

b() ->
    % 1
    {'div', 5, 3}.

c() ->
    % 3
    {'-', 4, b()}.

ac() ->
    %12
    {'*', a(), c()}.

d() ->
    % -1
    {'-', ac(), 13}.

e() ->
    % 6
    {'!', 3}.

de() ->
    % -6
    {'*', d(), e()}.

f() ->
    % 4
    {'*', 2, 2}.

g() ->
    % 12
    {'+', f(), h()}.

h() ->
    % 8
    {'*', 4, 2}.

i() ->
    % 6
    {'+', de(), g()}.

j() ->
    % 140
    {'.', [1, 2, 3], [10, 20, 30]}.

k() ->
    % 134
    {'-', j(), i()}.

l() ->
    % 15
    {'+', [0, 7, 8]}.

kl() ->
    % 119
    {'-', k(), l()}.

m() ->
    % - 119
    {'-', kl()}.

n() ->
    % 119
    {'-', m()}.

o() ->
    % - 119
    {'-', n()}.

p() ->
    % 5
    {'+', 3, 2}.

q() ->
    % 20
    {'+', 15, r()}.

r() ->
    % 5
    {'+', 1, 4}.

s() ->
    % 100
    {'*', p(), q()}.

t() ->
    % -19
    {'+', o(), s()}.

u() ->
    % 5
    {'+', 4, 1}.

v() ->
    % 120
    {'!', u()}.

w() ->
    % 1
    {'-', v(), 119}.

x() ->
    % - 18
    {'+', w(), t()}.

y() ->
    % 18
    {'-', x()}.

z() ->
    % -18
    {'-', y()}.
