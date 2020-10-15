-module(prime).

-export([start/0, is_prime/1, calculate_list/2]).

start() ->
    spawn(prime, calculate_list, num_range(2, 100)),
    spawn(prime, calculate_list, num_range(100, 400)),
    spawn(prime, calculate_list, num_range(500, 1500)),
    all_primes_test(2000) -
        all_primes_test(2000) +
        all_primes_test(2000) -
        all_primes_test(2000) +
        all_primes_test(2000).

num_range(A, Size) ->
    [A, A + Size].

is_prime(Num) ->
    test_prime(Num, 2).

test_prime(Num, I) when Num == I ->
    true;
test_prime(Num, I) ->
    if
        Num rem I == 0 ->
            false;
        true ->
            test_prime(Num, I + 1)
    end.

all_primes_test(UpTo) ->
    do_all_primes_test(2, UpTo, 2).

do_all_primes_test(N, UpTo, Last) when N == UpTo ->
    Last;
do_all_primes_test(N, UpTo, Last) ->
    case is_prime(N) of
        true ->
            do_all_primes_test(N + 1, UpTo, N);
        false ->
            do_all_primes_test(N + 1, UpTo, Last)
    end.

calculate_list(First, Last) when First < 2 ->
    calculate_list(First + 1, Last);
calculate_list(First, Last) when First == Last ->
    case is_prime(Last) of
        true ->
            Last;
        false ->
            []
    end;
calculate_list(First, Last) ->
    case is_prime(First) of
        true ->
            [First | calculate_list(First + 1, Last)];
        false ->
            calculate_list(First + 1, Last)
    end.
