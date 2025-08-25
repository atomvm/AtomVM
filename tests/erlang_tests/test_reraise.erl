-module(test_reraise).

-export([start/0]).

start() ->
    try
        reraise_reraiser:reraise_error()
    catch
        Class:Reason:Stacktrace ->
            error = Class,
            "foo" = Reason,

            [
                {reraise_raiser, raise_error, 0, _Meta1},
                {reraise_reraiser, reraise_error, 0, _Meta2}
                | _Rest
            ] = Stacktrace
    end,
    0.
