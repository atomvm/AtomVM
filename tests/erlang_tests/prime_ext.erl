-module (prime_ext).
-export([start/0, is_prime/1, calculate_list/2, test_prime/2, all_primes_test/1, do_all_primes_test/3]).

start() ->
    spawn(?MODULE, calculate_list, num_range(2, 100)),
    spawn(?MODULE, calculate_list, num_range(100, 400)),
    spawn(?MODULE, calculate_list, num_range(500, 1500)),
    ?MODULE:all_primes_test(2000) -
    ?MODULE:all_primes_test(2000) +
    ?MODULE:all_primes_test(2000) -
    ?MODULE:all_primes_test(2000) +
    ?MODULE:all_primes_test(2000).

num_range(A, Size) ->
    [A, A + Size].

is_prime(Num) ->
    ?MODULE:test_prime(Num, 2).

test_prime(Num, I) when Num == I ->
    true;
test_prime(Num, I)  ->
    if
        Num rem I == 0 ->
            false;

        true ->
            ?MODULE:test_prime(Num, I + 1)
    end.

all_primes_test(UpTo) ->
    ?MODULE:do_all_primes_test(2, UpTo, 2).

do_all_primes_test(N, UpTo, Last) when N == UpTo ->
    Last;
do_all_primes_test(N, UpTo, Last) ->
    case ?MODULE:is_prime(N) of
        true ->
            ?MODULE:do_all_primes_test(N + 1, UpTo, N);

        false ->
            ?MODULE:do_all_primes_test(N + 1, UpTo, Last)
    end.

calculate_list(First, Last) when First < 2 ->
    ?MODULE:calculate_list(First + 1, Last);
calculate_list(First, Last) when First == Last ->
    case ?MODULE:is_prime(Last) of
        true ->
            Last;
        false ->
            []
    end;
calculate_list(First, Last) ->
    case ?MODULE:is_prime(First) of
        true ->
            [First | ?MODULE:calculate_list(First + 1, Last)];
        false ->
            ?MODULE:calculate_list(First + 1, Last)
    end.
