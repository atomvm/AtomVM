%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(json_encoder).
-export([encode/1]).

encode(false) ->
    <<"false">>;

encode(true) ->
    <<"true">>;

encode(nil) ->
    <<"nil">>;

encode(null) ->
    <<"null">>;

encode(undefined) ->
    <<"null">>;

encode(Value) when is_atom(Value) ->
    [$", erlang:atom_to_binary(Value, latin1), $"];

encode(Value) when is_binary(Value) ->
    [$", Value, $"];

encode(Value) when is_float(Value) ->
    erlang:float_to_binary(Value, [{decimals, 32}, compact]);

encode(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);

encode(V) ->
    encode(V, []).

encode([{_K, _V} | _T] = L, []) ->
    encode(L, ${);

encode([{Key, Value} | []], Acc) ->
    Encoded = [$", encode_key(Key), "\":", encode(Value), $}],
    [Acc | Encoded];

encode([{Key, Value} | Tail], Acc) ->
    Encoded = [$", encode_key(Key), "\":", encode(Value), $,],
    encode(Tail, [Acc | Encoded]);

encode([_V | _T] = L, []) ->
    encode(L, $[);

encode([Value | []], Acc) ->
    Encoded = [encode(Value), $]],
    [Acc | Encoded];

encode([Value | Tail], Acc) ->
    Encoded = [encode(Value), $,],
    encode(Tail, [Acc | Encoded]).

encode_key(Key) when is_atom(Key) ->
    erlang:atom_to_binary(Key, latin1);

encode_key(Key) when is_binary(Key) ->
    Key.
