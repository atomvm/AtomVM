-module(sexp_parser).

-export([parse/1]).

parse([{'(', _Line} | T]) ->
    parse(T, []).

parse([{')', _Line}], Acc) ->
    reverse(Acc);

parse([{'(', _Line} | T], Acc) ->
    {NewTail, L} = parse(T, []),
    parse(NewTail, [L | Acc]);

parse([{')', _Line} | T], Acc) ->
    {T, reverse(Acc)};

parse([{symbol, _Line, Sym} | T], Acc) ->
    parse(T, [erlang:list_to_atom(Sym) | Acc]);

parse([{integer, _Line, Int} | T], Acc) ->
    parse(T, [Int | Acc]).

reverse(L) ->
    reverse(L, "").

reverse([], Acc) ->
    Acc;

reverse([H | T], Acc) ->
    reverse(T, [H | Acc]).
