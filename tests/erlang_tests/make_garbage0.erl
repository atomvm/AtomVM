-module(make_garbage0).
-export([start/0, compute/1, compute_tree/1, a/0, b/0, c/0, ac/0, d/0, e/0, de/0, f/0, g/0, h/0, i/0, j/0, k/0, l/0, kl/0, m/0, n/0, o/0, p/0, q/0, r/0, s/0, t/0]).

start() ->
    compute(t()).

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

dot([HA|TA], [HB|TB], Acc) ->
    dot(TA, TB, Acc + HA * HB).

sum_list([], Acc) ->
    Acc;

sum_list([H|T], Acc) ->
    sum_list(T, H + Acc).

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
