-module(copy_terms18).
-export([start/0, loop/0, compute/1, compute_tree/1, a/0, b/0, c/0, ac/0, d/0, e/0, de/0, f/0, g/0, h/0, i/0, j/0, k/0, l/0, kl/0, m/0, n/0, o/0, p/0, q/0, r/0, s/0, t/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
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

dot([HA|TA], [HB|TB], Acc) ->
    dot(TA, TB, Acc + HA * HB).

sum_list([], Acc) ->
    Acc;

sum_list([H|T], Acc) ->
    sum_list(T, H + Acc).

a() ->
    {'+', 1, 3, make_ref()}. % 4

b() ->
    {'div', 5, 3, make_ref()}. % 1

c() ->
    {'-', 4, b(), make_ref()}. % 3

ac() ->
    {'*', a(), c(), make_ref()}. %12

d() ->
    {'-', ac(), 13, make_ref()}. % -1

e() ->
    {'!', 3, make_ref()}. % 6

de() ->
    {'*', d(), e(), make_ref()}. % -6

f() ->
    {'*', 2, 2, make_ref()}. % 4

g() ->
    {'+', f(), h(), make_ref()}. % 12

h() ->
    {'*', 4, 2, make_ref()}. % 8

i() ->
    {'+', de(), g(), make_ref()}. % 6

j() ->
    {'.', [1, 2, 3], [10, 20, 30], make_ref()}. % 140

k() ->
    {'-', j(), i(), make_ref()}. % 134 - AtomVM crashes here

l() ->
    {'+', [0, 7, 8], make_ref()}. % 15

kl() ->
    {'-', k(), l(), make_ref()}. % 119

m() ->
    {'-', kl(), make_ref()}. % - 119

n() ->
    {'-', m(), make_ref()}. % 119

o() ->
    {'-', n(), make_ref()}. % - 119

p() ->
    {'+', 3, 2, make_ref()}. % 5

q() ->
    {'+', 15, r(), make_ref()}. % 20

r() ->
    {'+', 1, 4, make_ref()}. % 5

s() ->
    {'*', p(), q(), make_ref()}. % 100

t() ->
    {'+', o(), s(), make_ref()}. % -19
