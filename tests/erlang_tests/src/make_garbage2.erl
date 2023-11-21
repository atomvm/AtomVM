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

-module(make_garbage2).

-export([
    start/0,
    compute/1,
    compute_tree/1,
    a/1,
    b/0,
    c/0,
    ac/1,
    d/1,
    e/0,
    de/1,
    f/1,
    g/1,
    h/0,
    i/1,
    j/1,
    k/2,
    l/1,
    kl/2,
    m/2,
    n/2,
    o/2,
    p/1,
    q/1,
    r/1,
    s/1,
    t/3,
    u/0
]).

start() ->
    compute(u()).

compute(A) when is_tuple(A) ->
    compute(compute_tree(A));
compute(A) when is_number(A) ->
    A.

compute_tree({Op, A}) when is_tuple(A) ->
    {Op, compute_tree(A)};
compute_tree({Op, A, B}) when is_tuple(A) and is_number(B) ->
    {Op, compute_tree(A), B};
compute_tree({Op, A, B}) when is_number(A) and is_tuple(B) ->
    {Op, A, compute_tree(B)};
compute_tree({Op, A, B}) when is_tuple(A) and is_tuple(B) ->
    {Op, compute_tree(A), B};
% The following line also works, but it makes less iterations.
%   {Op, compute_tree(A), compute_tree(B)};

compute_tree({'!', A}) when is_number(A) ->
    fact(A);
compute_tree({'-', A}) when is_number(A) ->
    -A;
compute_tree({'+', A, B}) when is_number(A) and is_number(B) ->
    A + B;
compute_tree({'-', A, B}) when is_number(A) and is_number(B) ->
    A - B;
compute_tree({'*', A, B}) when is_number(A) and is_number(B) ->
    A * B;
compute_tree({'div', A, B}) when is_number(A) and is_number(B) ->
    A div B;
compute_tree({'.', A, B}) when is_list(A) and is_list(B) ->
    dot(A, B, 0);
compute_tree({'+', A}) when is_list(A) ->
    sum_list(A, 0);
compute_tree(A) when is_number(A) ->
    A.

fact(0) ->
    1;
fact(A) ->
    A * fact(A - 1).

dot([], [], Acc) ->
    Acc;
dot([HA | TA], [HB | TB], Acc) ->
    dot(TA, TB, Acc + HA * HB).

sum_list([], Acc) ->
    Acc;
sum_list([H | T], Acc) ->
    sum_list(T, H + Acc).

a(A) ->
    % 4
    A.

b() ->
    % 1
    {'div', 5, 3}.

c() ->
    % 3
    {'-', 4, b()}.

ac(A) ->
    %12
    {'*', a(A), c()}.

d(A) ->
    % -1
    {'-', ac(A), 13}.

e() ->
    % 6
    {'!', 3}.

de(A) ->
    % -6
    {'*', d(A), e()}.

f(A) ->
    % 4
    A.

g(A) ->
    % 12
    {'+', f(A), h()}.

h() ->
    % 8
    {'*', 4, 2}.

i(A) ->
    % 6
    {'+', de(A), g(A)}.

j(L) ->
    % 140
    {'.', L, [10, 20, 30]}.

k(L, A) ->
    % 134
    {'-', j(L), i(A)}.

l(L) ->
    % 15
    {'+', L}.

kl(L, A) ->
    % 119
    {'-', k(L, A), l(L)}.

m(L, A) ->
    % - 119
    {'-', kl(L, A)}.

n(L, A) ->
    % 119
    {'-', m(L, A)}.

o(L, A) ->
    % - 119
    {'-', n(L, A)}.

p(R) ->
    % 5
    R.

q(R) ->
    % 20
    {'+', 15, r(R)}.

r(R) ->
    % 5
    R.

s(R) ->
    % 100
    {'*', p(R), q(R)}.

t(L, A, R) ->
    % -19
    {'+', o(L, A), s(R)}.

u() ->
    L = [1, 2, 3],
    A = {'+', 1, 3},
    R = {'+', 1, 4},
    {'+', t(L, A, R), t(L, A, R)}.
