-module(network).

-export([open/0, setup/2]).

open() ->
    open_port({spawn, "network"}, []).

setup(Network, Config) ->
    Network ! {self(), make_ref(), setup, Config},
    receive
        Ret ->
            Ret
    end.
