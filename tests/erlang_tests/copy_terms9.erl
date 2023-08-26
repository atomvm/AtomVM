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

-module(copy_terms9).

-export([
    start/0,
    compute/1,
    compute_tree/1,
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
    t/0
]).

start() ->
    Pid = spawn_opt(fun loop/0, []),
    Pid ! {self(), t()},
    Res =
        receive
            Any -> Any
        end,
    Pid ! terminate,
    Res.

loop() ->
    case handle_request() of
        terminate ->
            terminate;
        ok ->
            loop()
    end.

handle_request() ->
    receive
        {Pid, Tree} ->
            Pid ! compute(Tree),
            ok;
        terminate ->
            terminate
    end.

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
    % 134 - AtomVM crashes here
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
