-module(pingpong).

-export([start/0, ping/2, pong/2]).

start() ->
    Echo = echo:start(),
    Pong = spawn(?MODULE, pong, [Echo, self()]),
    spawn(?MODULE, ping, [Echo, Pong]),
    receive
        exit -> 1;
        _Any -> 0
    end.

ping(Echo, Pong) ->
    ping(Echo, Pong, 0).

ping(_Echo, Pong, 10000) ->
    Pong ! exit;

ping(Echo, Pong, N) ->
    Pong ! {self(), ping, f(N, N)},
    receive
        {Pong, pong, M} ->
            echo:puts("+"),
            ping(Echo, Pong, M)
    end.

pong(Echo, Main) ->
    receive
        {Ping, ping, N} ->
            echo:puts("-"),
            Ping ! {self(), pong, f(N, N + 1)},
            pong(Echo, Main);
        exit ->
            Main ! exit
    end.

f(N, M) when N < 1 ->
    M;

f(N, M) ->
    f((N - 1) rem 16, M).
