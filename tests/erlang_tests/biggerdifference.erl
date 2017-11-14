-module(biggerdifference).

-export([start/0, willnotbecalled/1, willnotbecalled/2]).

start() ->
    diff1500(500) div 2.

diff1500(A) ->
    1500 - double(A).

double(0) ->
    0;
double(I) ->
    id(I)*2.

id(0) ->
    0;
id(I) ->
    I.

willnotbecalled(0) ->
    2047;
willnotbecalled(I) ->
    2000 div I.

willnotbecalled(A, B) ->
    (A - B)*2 + 2047.
