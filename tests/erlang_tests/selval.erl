-module(selval).
-export([start/0,selval/1]).

start() -> selval(2).

selval(0) ->
    7;
selval(1) ->
    8;
selval(2) ->
    9;
selval(7) ->
    10;
selval(_) ->
    0.
