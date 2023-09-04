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

-module(copy_terms18).

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

compute_tree({Op, A, Ref}) when is_tuple(A) and is_reference(Ref) ->
    {Op, compute_tree(A), Ref};
compute_tree({Op, A, B, Ref}) when is_tuple(A) and is_number(B) and is_reference(Ref) ->
    {Op, compute_tree(A), B, Ref};
compute_tree({Op, A, B, Ref}) when is_number(A) and is_tuple(B) and is_reference(Ref) ->
    {Op, A, compute_tree(B), Ref};
compute_tree({Op, A, B, Ref}) when is_tuple(A) and is_tuple(B) and is_reference(Ref) ->
    {Op, compute_tree(A), B, Ref};
% The following line also works, but it makes less iterations.
%   {Op, compute_tree(A), compute_tree(B)};

compute_tree({'!', A, Ref}) when is_number(A) and is_reference(Ref) ->
    fact(A);
compute_tree({'-', A, Ref}) when is_number(A) and is_reference(Ref) ->
    -A;
compute_tree({'+', A, B, Ref}) when is_number(A) and is_number(B) and is_reference(Ref) ->
    A + B;
compute_tree({'-', A, B, Ref}) when is_number(A) and is_number(B) and is_reference(Ref) ->
    A - B;
compute_tree({'*', A, B, Ref}) when is_number(A) and is_number(B) and is_reference(Ref) ->
    A * B;
compute_tree({'div', A, B, Ref}) when is_number(A) and is_number(B) and is_reference(Ref) ->
    A div B;
compute_tree({'.', A, B, Ref}) when is_list(A) and is_list(B) and is_reference(Ref) ->
    dot(A, B, 0);
compute_tree({'+', A, Ref}) when is_list(A) and is_reference(Ref) ->
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
    {'+', 1, 3, make_ref()}.

b() ->
    % 1
    {'div', 5, 3, make_ref()}.

c() ->
    % 3
    {'-', 4, b(), make_ref()}.

ac() ->
    %12
    {'*', a(), c(), make_ref()}.

d() ->
    % -1
    {'-', ac(), 13, make_ref()}.

e() ->
    % 6
    {'!', 3, make_ref()}.

de() ->
    % -6
    {'*', d(), e(), make_ref()}.

f() ->
    % 4
    {'*', 2, 2, make_ref()}.

g() ->
    % 12
    {'+', f(), h(), make_ref()}.

h() ->
    % 8
    {'*', 4, 2, make_ref()}.

i() ->
    % 6
    {'+', de(), g(), make_ref()}.

j() ->
    % 140
    {'.', [1, 2, 3], [10, 20, 30], make_ref()}.

k() ->
    % 134 - AtomVM crashes here
    {'-', j(), i(), make_ref()}.

l() ->
    % 15
    {'+', [0, 7, 8], make_ref()}.

kl() ->
    % 119
    {'-', k(), l(), make_ref()}.

m() ->
    % - 119
    {'-', kl(), make_ref()}.

n() ->
    % 119
    {'-', m(), make_ref()}.

o() ->
    % - 119
    {'-', n(), make_ref()}.

p() ->
    % 5
    {'+', 3, 2, make_ref()}.

q() ->
    % 20
    {'+', 15, r(), make_ref()}.

r() ->
    % 5
    {'+', 1, 4, make_ref()}.

s() ->
    % 100
    {'*', p(), q(), make_ref()}.

t() ->
    % -19
    {'+', o(), s(), make_ref()}.
