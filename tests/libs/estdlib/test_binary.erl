%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_binary).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_split(),
    ok.

test_split() ->
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"oo">>), [<<"f">>, <<"bar">>]),
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"ooz">>), [<<"foobar">>]),
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"o">>), [<<"f">>, <<"obar">>]),
    ?ASSERT_MATCH(binary:split(<<"foobar">>, <<"o">>, [global]), [<<"f">>, <<>>, <<"bar">>]),
    ok.
