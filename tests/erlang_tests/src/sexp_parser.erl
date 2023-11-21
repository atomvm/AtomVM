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
