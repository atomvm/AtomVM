-module(spawn_fun2).

-export([start/0]).

start() ->
    Father = self(),
    Fun = fun() -> Father ! 33 end,
    spawn(Fun),
    Result =
        receive
            Any -> Any
        end,
    Result.
