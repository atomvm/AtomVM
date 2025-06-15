%
% This file is part of AtomVM.
%
% Copyright 2024 Tomasz Sobkiewicz <tomasz.sobkiewicz@swmansion.com>
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

-module(test_binary_replace).

-export([start/0]).

start() ->
    ok = replace(),
    ok = global_replace(),
    ok = invalid_args(),
    0.

replace() ->
    <<"">> = binary:replace(<<"">>, <<"">>, <<"">>),
    <<"barbar">> = binary:replace(<<"foobar">>, <<"foo">>, <<"bar">>),
    <<"foooobar">> = binary:replace(<<"foobar">>, <<"o">>, <<"ooo">>),
    <<"">> = binary:replace(<<"foobar">>, <<"foobar">>, <<"">>),
    <<"foobar">> = binary:replace(<<"o">>, <<"o">>, <<"foobar">>),
    <<"fof">> = binary:replace(<<"foobar">>, <<"obar">>, <<"f">>),
    <<"fobar">> = binary:replace(<<"foobar">>, <<"oo">>, <<"o">>),
    <<"o">> = binary:replace(<<"o">>, <<"foobar">>, <<"o">>),
    <<"foobar">> = binary:replace(<<"o">>, <<"o">>, <<"foobar">>),
    <<"fobar">> = binary:replace(<<"foobar">>, <<"oo">>, <<"o">>, []),
    <<"foo">> = binary:replace(<<"foofoo">>, <<"foo">>, <<"">>, []),
    ok.

global_replace() ->
    <<"">> = binary:replace(<<"">>, <<"">>, <<"">>, [global]),
    <<"foobar">> = binary:replace(<<"foooobar">>, <<"oo">>, <<"o">>, [global]),
    <<"foooobar">> = binary:replace(<<"foobar">>, <<"o">>, <<"oo">>, [global]),
    <<"">> = binary:replace(<<"foofoo">>, <<"foo">>, <<"">>, [global]),
    <<"foofoo">> = binary:replace(<<"oo">>, <<"o">>, <<"foo">>, [global]),
    ok.

invalid_args() ->
    ok = raises(badarg, fun() -> binary:replace(<<"o">>, <<"">>, <<"">>) end),
    ok = raises(badarg, fun() -> binary:replace(not_binary, <<"">>, <<"">>) end),
    ok = raises(badarg, fun() -> binary:replace(<<"o">>, not_binary, <<"">>) end),
    ok = raises(badarg, fun() -> binary:replace(<<"o">>, <<"">>, not_binary) end),
    ok = raises(badarg, fun() -> binary:replace(<<"o">>, <<"">>, <<"">>, [global]) end),
    ok = raises(badarg, fun() -> binary:replace(<<"o">>, <<"o">>, <<"">>, [{global, true}]) end),
    % insert_replaced not supported
    ok = raises(badarg, fun() -> binary:replace(<<"o">>, <<"o">>, <<"">>, [global, {insert_replaced, 1}]) end),
    ok = raises(badarg, fun() -> binary:replace(<<"o">>, <<"o">>, <<"">>, [{insert_replaced, 1}]) end),
    ok.

raises(Error, F) ->
    try F() of
        V ->
            {unexpected, V}
    catch
        error:Error -> ok;
        C:E -> {unexpected, C, E}
    end.
