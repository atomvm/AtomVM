-module(moreintegertests).

-export([
    start/0,
    func5_test/5,
    square/1,
    not_used1/4,
    not_used2/1,
    not_used3/2,
    not_used4/4,
    not_used5/1,
    not_used6/0
]).

start() ->
    minus1000(func5_test(2000, 1000, 2, 4, 1999)).

func5_test(A, B, C, D, E) ->
    (((return1023_if_1(A - E) div return1023_if_1(id(A) - id(E))) * C) * square(D)) + B.

id(0) ->
    0;
id(A) ->
    A.

not_used1(A, B, C, D) ->
    (A - B) div (C - D).

not_used2(A) ->
    A * 2.

not_used3(A, B) ->
    A div (B + 1).

not_used4(A, B, C, D) ->
    not_used1(not_used3(A, B), not_used2(C), not_used1(100, 2, D, 600), 1024).

return1023_if_1(0) ->
    0;
return1023_if_1(_A) ->
    1023.

not_used5(0) ->
    999;
not_used5(A) ->
    A * 2047.

not_used6() ->
    255.

square(0) ->
    0;
square(X) ->
    X * X.

minus1000(0) ->
    0;
minus1000(A) ->
    A - 1000.
