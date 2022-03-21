%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(sexp_lexer).

-export([string/1]).

string(L) ->
    string(L, 1).

string([], _Line) ->
    [];
string([$\n | T], Line) ->
    string(T, Line + 1);
string([$( | T], Line) ->
    [{'(', Line} | string(T, Line)];
string([$) | T], Line) ->
    [{')', Line} | string(T, Line)];
string([$\s | T], Line) ->
    string(T, Line);
string([$- | _T] = L, Line) ->
    integer(L, "", Line);
string([C | _T] = L, Line) when C >= $0 andalso C =< $9 ->
    integer(L, "", Line);
string(L, Line) ->
    symbol(L, "", Line).

integer([], Acc, Line) ->
    make_integer([], Acc, Line);
integer([$\s | T], Acc, Line) ->
    make_integer(T, Acc, Line);
integer([$\t | T], Acc, Line) ->
    make_integer(T, Acc, Line);
integer([$\n | T], Acc, Line) ->
    make_integer(T, Acc, Line);
integer([$( | _T] = L, Acc, Line) ->
    make_integer(L, Acc, Line);
integer([$) | _T] = L, Acc, Line) ->
    make_integer(L, Acc, Line);
integer([C | T], Acc, Line) ->
    integer(T, [C | Acc], Line).

make_integer(T, Acc, Line) ->
    IntegerList = reverse(Acc),
    Integer = erlang:list_to_integer(IntegerList),
    [{integer, Line, Integer} | string(T, Line)].

symbol([], Acc, Line) ->
    make_symbol([], Acc, Line);
symbol([$\s | T], Acc, Line) ->
    make_symbol(T, Acc, Line);
symbol([$\t | T], Acc, Line) ->
    make_symbol(T, Acc, Line);
symbol([$\n | T], Acc, Line) ->
    make_symbol(T, Acc, Line);
symbol([$( | _T] = L, Acc, Line) ->
    make_symbol(L, Acc, Line);
symbol([$) | _T] = L, Acc, Line) ->
    make_symbol(L, Acc, Line);
symbol([C | T], Acc, Line) ->
    symbol(T, [C | Acc], Line).

make_symbol(T, Acc, Line) ->
    Symbol = reverse(Acc),
    [{symbol, Line, Symbol} | string(T, Line)].

reverse(L) ->
    reverse(L, "").

reverse([], Acc) ->
    Acc;
reverse([H | T], Acc) ->
    reverse(T, [H | Acc]).
