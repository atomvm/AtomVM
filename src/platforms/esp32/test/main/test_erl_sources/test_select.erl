%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_select).

-export([start/0]).

% This test relies on a special vfs registered under /pipe.

% Timeout has been changed from 200 to 2000 after a number of failures on EPS32-P4 devices

start() ->
    {ok, WrFd} = atomvm:posix_open("/pipe/0", [o_wronly]),
    {ok, RdFd} = atomvm:posix_open("/pipe/0", [o_rdonly]),
    % Make sure this test vfs works as expected
    {ok, 1} = atomvm:posix_write(WrFd, <<42>>),
    {error, eagain} = atomvm:posix_write(WrFd, <<43>>),
    {ok, <<42>>} = atomvm:posix_read(RdFd, 1),
    {error, eagain} = atomvm:posix_read(RdFd, 1),

    % Write fd should be selectable.
    SelectWriteRef = make_ref(),
    ok = atomvm:posix_select_write(WrFd, self(), SelectWriteRef),
    ok =
        receive
            {select, WrFd, SelectWriteRef, ready_output} -> ok;
            M -> {unexpected, M}
        after 2000 -> {timeout, ?MODULE, ?LINE}
        end,
    ok = atomvm:posix_select_stop(WrFd),

    % Write and check that rd is selectable fd should be selectable.
    {ok, 1} = atomvm:posix_write(WrFd, <<42>>),
    SelectReadRef = make_ref(),
    ok = atomvm:posix_select_read(RdFd, self(), SelectReadRef),
    ok =
        receive
            {select, RdFd, SelectReadRef, ready_input} -> ok
        after 2000 -> {timeout, ?MODULE, ?LINE}
        end,
    {ok, <<42>>} = atomvm:posix_read(RdFd, 1),
    ok = atomvm:posix_select_read(RdFd, self(), SelectReadRef),
    ok =
        receive
            {select, RdFd, SelectReadRef, _} -> {unexpected, ?MODULE, ?LINE}
        after 2000 -> ok
        end,
    {ok, 1} = atomvm:posix_write(WrFd, <<43>>),
    ok =
        receive
            {select, RdFd, SelectReadRef, ready_input} -> ok;
            M2 -> {unexpected, M2}
        after 2000 -> {timeout, ?MODULE, ?LINE}
        end,
    ok = atomvm:posix_select_stop(RdFd),
    ok =
        receive
            Message -> {unexpected, Message, ?MODULE, ?LINE}
        after 2000 -> ok
        end,

    ok = atomvm:posix_close(WrFd),
    ok = atomvm:posix_close(RdFd),
    ok.
