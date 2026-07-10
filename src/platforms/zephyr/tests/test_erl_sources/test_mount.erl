%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_mount).
-export([start/0]).

start() ->
    try
        ok = test_mkfs_and_mount(),
        ok
    catch
        Class:Reason:Stack ->
            erlang:display({Class, Reason, Stack}),
            error
    end.

test_mkfs_and_mount() ->
    SystemArchitecture = erlang:system_info(system_architecture),
    IsESP32 = case binary:split(SystemArchitecture, <<"-">>, [global]) of
        [<<"xtensa">>, Vendor | _] ->
            nomatch =/= binary:match(Vendor, <<"espressif">>);
        _ ->
            false
    end,
    DeviceName = if IsESP32 -> "SD"; true -> "RAM" end,
    MountPt = "/" ++ DeviceName ++ ":",
    FilePath = MountPt ++ "/test.txt",
    ok =
        case DeviceName of
            "RAM" -> zephyr:mkfs(DeviceName, fat);
            _ -> ok
        end,
    {ok, Ref} = zephyr:mount(DeviceName, MountPt, fat, []),

    {ok, Fd} = atomvm:posix_open(FilePath, [o_rdwr, o_creat], 8#644),
    BytesWritten = atomvm:posix_write(Fd, <<"Hello Zephyr Storage!">>),
    true = (BytesWritten > 0),
    ok = atomvm:posix_close(Fd),

    {ok, Fd2} = atomvm:posix_open(FilePath, [o_rdonly], 8#644),
    {ok, <<"Hello Zephyr Storage!">>} = atomvm:posix_read(Fd2, 50),
    ok = atomvm:posix_close(Fd2),

    ok = zephyr:umount(Ref),

    {error, _} = atomvm:posix_open(FilePath, [o_rdonly], 8#644),
    ok.
