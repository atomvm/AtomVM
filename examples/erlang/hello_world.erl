-module (hello_world).
-export([start/0]).

start() ->
    console:puts("Hello World\n").
