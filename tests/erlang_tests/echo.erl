-module(echo).

-export([start/0, puts/1, puts/2]).

puts(String) ->
    puts(get_pid(), String).

puts(Echo, String) ->
    call(Echo, String).

%% Internal operations

call(Echo, Msg) ->
    Echo ! {self(), Msg},
    receive
        Any -> Any
    end.

get_pid() ->
    case whereis(echo) of
        undefined ->
            start();
        Any ->
            Any
    end.

start() ->
    Pid = erlang:open_port({spawn, "echo"}, []),
    erlang:register(echo, Pid),
    Pid.
