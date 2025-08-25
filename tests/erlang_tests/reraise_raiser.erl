-module(reraise_raiser).

-export([raise_error/0]).

raise_error() ->
    error("foo").
