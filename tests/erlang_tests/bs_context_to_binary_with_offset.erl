%
% This file is part of AtomVM.
%
% Copyright 2022 Davide Bettio <davide@uninstall.it>
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

-module(bs_context_to_binary_with_offset).
-export([parse/1, start/0]).

start() ->
    Json = <<"{\"foo\": \"bar\", \"four\": 4}">>,
    parse(Json).

parse(<<>>) ->
    42;
parse(<<$\s, Rest/binary>>) ->
    parse(Rest);
parse(<<${, Rest/binary>>) ->
    parse(Rest);
parse(<<$}, Rest/binary>>) ->
    parse(Rest);
parse(<<$", Rest/binary>>) ->
    parse_string(Rest, 1);
parse(<<$:, Rest/binary>>) ->
    parse(Rest);
parse(<<$,, Rest/binary>>) ->
    parse(Rest);
parse(<<Digit, _Rest/binary>> = Bin) when Digit >= $0 andalso Digit =< $9 ->
    parse_number(Bin, 0).

parse_string(In, Len) when is_binary(In) and is_integer(Len) ->
    case In of
        <<_S:Len/binary, $", Rest/binary>> ->
            parse(Rest);
        <<_S:Len/binary, _Rest/binary>> ->
            parse_string(In, Len + 1);
        Truncated ->
            {cont, string, Truncated, Len}
    end.

parse_number(<<>>, Acc) ->
    Acc;
parse_number(<<Digit, Rest/binary>>, Acc) when Digit >= $0 andalso Digit =< $9 ->
    parse_number(Rest, Acc * 10 + (Digit - $0));
parse_number(Bin, _Acc) ->
    parse(Bin).
