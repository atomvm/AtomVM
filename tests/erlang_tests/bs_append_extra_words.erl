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

-module(bs_append_extra_words).

-export([start/0]).

start() ->
    {ok, Parsed} = parse_digits(<<"234907.00">>),
    case binary_to_list(Parsed) of
        "234907" -> 1;
        _ -> -1
    end.

parse_digits(In) ->
    parse_digits(In, 1).

parse_digits(In, Len) when is_binary(In) and is_integer(Len) ->
    case In of
        <<S1:Len/binary, S2:2/binary, $., _Rest/binary>> ->
            {ok, <<S1/binary, S2/binary>>};
        <<_S:Len/binary, _Rest/binary>> ->
            parse_digits(In, Len + 1);
        _Invalid ->
            {ok, <<"">>}
    end.
