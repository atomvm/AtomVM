-module(reraise_reraiser).

-export([reraise_error/0]).

reraise_error() ->
    try
        reraise_raiser:raise_error()
    catch
        Class:Reason:Stacktrace ->
            erlang:error(erlang:raise(Class, Reason, Stacktrace))
    end.
