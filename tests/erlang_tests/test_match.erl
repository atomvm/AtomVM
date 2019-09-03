-module(test_match).
-export([start/0, echo/1]).

start() ->
    Term = {a,b,c},
    Term = echo(Term),
    0.

echo(Term) ->
    Term.
