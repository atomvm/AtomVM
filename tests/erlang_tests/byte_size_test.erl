-module (byte_size_test).
-export([start/0,bsize/2]).

start() ->
    bsize(<<"I">>, 0) + bsize(<<"am">>, 0) + bsize(<<"a">>, 0) + bsize(<<"binary">>, 0).

bsize(Bin, V) ->
    byte_size(Bin) + V.
