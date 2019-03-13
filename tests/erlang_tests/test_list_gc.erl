-module(test_list_gc).

-export([start/0]).

start() ->
    Self = self(),
    Config = [
        {foo, [{bar, Self}]},
        {tapas, 30000}
    ],
    erlang:display(Config),
    length(Config).
