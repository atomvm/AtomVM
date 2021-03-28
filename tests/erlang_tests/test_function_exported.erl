-module(test_function_exported).
-export([start/0, c/2, fail/3]).

start() ->
    fail(id(erlang), id(5), 1) +
        fail(id(5), id(display), 1) +
        fail(id(erlang), id(display), foo) +
        c(erlang:function_exported(erlang, display, 1), true) +
        c(erlang:function_exported(erlang, display, 2), false) +
        c(erlang:function_exported(erlang, displayz, 1), false) +
        c(erlang:function_exported(foo, display, 1), false) +
        c(erlang:function_exported(?MODULE, c, 1), false) +
        c(erlang:function_exported(?MODULE, c, 2), true) +
        c(erlang:function_exported(?MODULE, foo, 1), false).

c(A, A) when is_boolean(A) ->
    1;
c(A, B) when is_boolean(A) andalso is_boolean(B) ->
    0.

fail(Module, Function, Arity) ->
    try erlang:function_exported(Module, Function, Arity) of
        _A -> 1
    catch
        error:badarg -> 0
    end.

id(A) ->
    A.
