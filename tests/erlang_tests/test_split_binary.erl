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

-module(test_split_binary).

-export([
    start/0,
    id/1
]).

start() ->
    ok = test_split_binary(),
    0.

test_split_binary() ->
    {<<"a">>, <<"bcde">>} = split_binary(?MODULE:id(<<"abcde">>), 1),
    {<<>>, <<"abcde">>} = split_binary(?MODULE:id(<<"abcde">>), 0),
    {<<>>, <<>>} = split_binary(?MODULE:id(<<>>), 0),
    {<<"abcde">>, <<>>} = split_binary(?MODULE:id(<<"abcde">>), 5),
    ok =
        try
            _ = split_binary(?MODULE:id(<<"abcde">>), 6),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            _ = split_binary(?MODULE:id(<<"abcde">>), -1),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok =
        try
            _ = split_binary(?MODULE:id(<<>>), 1),
            unexpected
        catch
            error:badarg -> ok
        end,
    ok.

id(X) -> X.
