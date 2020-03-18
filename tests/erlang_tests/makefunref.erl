-module(makefunref).

-export([start/0, mfa2fun/3, id/1]).

start() ->
    F = mfa2fun(erlang, integer_to_list, id(0)),
    L = F(id(123)),
    id(length(L)).

mfa2fun(Module, FunctionName, TheArity) ->
    Arity = TheArity + 1,
    fun Module:FunctionName/Arity.

id(X) when is_integer(X) ->
    X * 1.
