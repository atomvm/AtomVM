%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_select).

-export([start/0]).

start() ->
    try
        ok = test_socketpair_select(),
        ok
    catch
        Class:Reason:Stack ->
            erlang:display({Class, Reason, Stack}),
            error
    end.

test_socketpair_select() ->
    {ok, {WrFd, RdFd}} = zephyr:socketpair(),

    % Write to WrFd, verify RdFd receives the byte
    {ok, 5} = atomvm:posix_write(WrFd, <<"Hello">>),
    {ok, <<"Hello">>} = atomvm:posix_read(RdFd, 5),

    % Read fd should be selectable (blocking/ready)
    SelectReadRef = make_ref(),
    ok = atomvm:posix_select_read(RdFd, self(), SelectReadRef),
    % It shouldn't trigger yet because there is no data
    receive
        {select, RdFd, SelectReadRef, _} ->
            exit(should_not_trigger_yet)
    after 200 ->
        ok
    end,

    % Now write a byte to WrFd, and it should trigger read notification!
    {ok, 1} = atomvm:posix_write(WrFd, <<42>>),
    ok = receive
        {select, RdFd, SelectReadRef, ready_input} ->
            ok
    after 2000 ->
        exit(timeout_waiting_for_input)
    end,

    % Read the byte so it goes back to empty
    {ok, <<42>>} = atomvm:posix_read(RdFd, 1),

    % Stop selecting on RdFd
    ok = atomvm:posix_select_stop(RdFd),

    % Write fd should also be selectable (ready to output)
    SelectWriteRef = make_ref(),
    ok = atomvm:posix_select_write(WrFd, self(), SelectWriteRef),
    ok = receive
        {select, WrFd, SelectWriteRef, ready_output} ->
            ok
    after 2000 ->
        exit(timeout_waiting_for_output)
    end,
    ok = atomvm:posix_select_stop(WrFd),

    ok = atomvm:posix_close(WrFd),
    ok = atomvm:posix_close(RdFd),
    ok.
