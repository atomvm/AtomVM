%%
%% This file is part of AtomVM.
%%
%% Copyright (c) 2025 <winford@object.stream>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
-module(test_port_to_list).

-export([start/0, test_echo_port_to_list/0]).

-define(PORTBEGIN, "#Port<0.").
-define(PORTEND, ">").

start() ->
    test_echo_port_to_list().

test_echo_port_to_list() ->
    Port = open_port({spawn, "echo"}, []),
    validate_port_fmt(Port).

%% internal
validate_port_fmt(Port) ->
    case get_valid_portchars(Port) of
        {ok, PortChars} ->
            Res = port_to_list(Port) =:= flatten([?PORTBEGIN, PortChars, ?PORTEND], []),
            case Res of
                true -> 0;
                _ -> 2
            end;
        {invalid_format, Port} ->
            1
    end.

get_valid_portchars(Port) ->
    PList = port_to_list(Port),
    Bin = list_to_binary(PList),
    [_, Portend] = binary:split(Bin, <<?PORTBEGIN>>),
    [PortNum, _] = binary:split(Portend, <<?PORTEND>>),
    try binary_to_integer(PortNum) of
        Num when is_integer(Num) ->
            {ok, binary_to_list(PortNum)};
        _ ->
            {invalid_format, Port}
    catch
        _:_ ->
            {invalid_format, Port}
    end.

flatten([], Accum) ->
    Accum;
flatten([H | T], Accum) when is_list(H) ->
    FlattenedT = flatten(T, Accum),
    flatten(H, FlattenedT);
flatten([H | T], Accum) ->
    FlattenedT = flatten(T, Accum),
    [H | FlattenedT].
