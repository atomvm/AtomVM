-module(lowercase).
-export([start/0, lowercase/1]).

start() ->
    (hd(lowercase("N")) - 109) +
    (hd(lowercase("Z")) - 121) * 2 +
    (hd(lowercase("A")) - 96) * 4 +
    (hd(lowercase("d")) - 99) * 8.

lowercase("A") ->
    "a";
lowercase("B") ->
    "b";
lowercase("C") ->
    "c";
lowercase("D") ->
    "d";
lowercase("E") ->
    "e";
lowercase("F") ->
    "f";
lowercase("G") ->
    "g";
lowercase("H") ->
    "h";
lowercase("I") ->
    "i";
lowercase("J") ->
    "j";
lowercase("K") ->
    "k";
lowercase("L") ->
    "l";
lowercase("M") ->
    "m";
lowercase("N") ->
    "n";
lowercase("O") ->
    "o";
lowercase("P") ->
    "p";
lowercase("Q") ->
    "q";
lowercase("R") ->
    "r";
lowercase("S") ->
    "s";
lowercase("T") ->
    "t";
lowercase("U") ->
    "u";
lowercase("V") ->
    "v";
lowercase("W") ->
    "w";
lowercase("X") ->
    "x";
lowercase("Y") ->
    "y";
lowercase("Z") ->
    "z";
lowercase(Any) ->
    Any.
