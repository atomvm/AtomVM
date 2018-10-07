-module(is_type).
-export([start/0, test_is_type/8, all_true/8, quick_exit/0]).

start() ->
    Pid = spawn(?MODULE, quick_exit, []),
    test_is_type(hello, <<"hello">>, 10, [1, 2, 3], 5, Pid, make_ref(), {1, 2}).

test_is_type(A, B, I, L, N, P, R, T) ->
    all_true(is_atom(A), is_binary(B), is_integer(I), is_list(L), is_number(N), is_pid(P), is_reference(R), is_tuple(T)).

all_true(true, true, true, true, true, true, true, true) ->
    255;
all_true(false, false, false, false, false, false, false, false) ->
    0;
all_true(_, _, _, _, _, _, _, _) ->
    -1.

quick_exit() ->
    ok.
