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

-module(test_inet).

-export([test/0]).

test() ->
    ok = test_getaddr(),
    ok.

test_getaddr() ->
    {ok, {127, 0, 0, 1}} = inet:getaddr(localhost, inet),
    {ok, {127, 0, 0, 1}} = inet:getaddr("localhost", inet),
    {ok, {_, _, _, _}} = inet:getaddr("test.atomvm.org", inet),
    % RFC8880
    {ok, {192, 0, 0, LastByte}} = inet:getaddr("ipv4only.arpa", inet),
    true = LastByte =:= 170 orelse LastByte =:= 171,
    {error, einval} = inet:getaddr(127, inet),
    {error, einval} = inet:getaddr({127.0, 0, 0, 1.0}, inet),
    {error, einval} = inet:getaddr({312, 0, 0, 1}, inet),
    {error, einval} = inet:getaddr({foo, bar}, inet),
    {error, einval} = inet:getaddr(<<"localhost">>, inet),
    {error, _} = inet:getaddr("localhost.invalid", inet),
    ok.
