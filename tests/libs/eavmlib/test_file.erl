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

-module(test_file).

-export([test/0]).

-include("etest.hrl").

test() ->
    HasSelect = atomvm:platform() =/= emscripten,
    HasExecve = atomvm:platform() =/= emscripten,
    ok = test_basic_file(),
    ok = test_fifo_select(HasSelect),
    ok = test_gc(HasSelect),
    ok = test_crash_no_leak(HasSelect),
    ok = test_select_with_gone_process(HasSelect),
    ok = test_select_with_listeners(HasSelect),
    ok = test_seek(),
    ok = test_pread_pwrite(),
    ok = test_fsync(),
    ok = test_ftruncate(),
    ok = test_rename(),
    ok = test_stat(),
    ok = test_fstat(),
    ok = test_mkdir_rmdir(),
    ok = test_validation(),
    ok = test_subprocess(HasExecve),
    ok.

test_basic_file() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_wronly, o_creat], 8#644),
    {ok, 5} = atomvm:posix_write(Fd, <<"Hello">>),
    ok = atomvm:posix_close(Fd),
    {ok, Fd2} = atomvm:posix_open(Path, [o_rdwr]),
    {ok, <<"He">>} = atomvm:posix_read(Fd2, 2),
    {ok, <<"llo">>} = atomvm:posix_read(Fd2, 10),
    eof = atomvm:posix_read(Fd2, 10),
    {ok, 6} = atomvm:posix_write(Fd2, <<" World">>),
    eof = atomvm:posix_read(Fd2, 10),
    ok = atomvm:posix_close(Fd2),
    ok = atomvm:posix_unlink(Path).

test_fifo_select(false) ->
    ok;
test_fifo_select(_HasSelect) ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    ok = atomvm:posix_mkfifo(Path, 8#644),
    {ok, RdFd} = atomvm:posix_open(Path, [o_rdonly]),
    {ok, WrFd} = atomvm:posix_open(Path, [o_wronly]),
    SelectWriteRef = make_ref(),
    ok = atomvm:posix_select_write(WrFd, self(), SelectWriteRef),
    ok =
        receive
            {select, WrFd, SelectWriteRef, ready_output} -> ok;
            M -> {unexpected, M}
        after 200 -> fail
        end,
    ok = atomvm:posix_select_stop(WrFd),
    {ok, 5} = atomvm:posix_write(WrFd, <<"Hello">>),
    SelectReadRef = make_ref(),
    ok = atomvm:posix_select_read(RdFd, self(), SelectReadRef),
    ok =
        receive
            {select, RdFd, SelectReadRef, ready_input} -> ok
        after 200 -> fail
        end,
    {ok, <<"Hello">>} = atomvm:posix_read(RdFd, 10),
    ok = atomvm:posix_select_read(RdFd, self(), SelectReadRef),
    ok =
        receive
            {select, RdFd, SelectReadRef, _} -> fail
        after 200 -> ok
        end,
    {ok, 6} = atomvm:posix_write(WrFd, <<" World">>),
    ok =
        receive
            {select, RdFd, SelectReadRef, ready_input} -> ok;
            M2 -> {unexpected, M2}
        after 200 -> fail
        end,
    Parent = self(),
    Child = spawn(fun() ->
        ChildRef = make_ref(),
        ok = atomvm:posix_select_read(RdFd, self(), ChildRef),
        ok =
            receive
                {select, RdFd, ChildRef, ready_input} -> ok;
                M3 -> {unexpected, M3}
            after 200 -> fail
            end,
        ok = atomvm:posix_select_stop(RdFd),
        {ok, <<" World">>} = atomvm:posix_read(RdFd, 10),
        Parent ! {self(), world}
    end),
    ok =
        receive
            {Child, world} -> ok;
            M4 -> {unexpected, M4}
        after 200 -> fail
        end,
    ok =
        receive
            Message -> {unexpected, Message}
        after 200 -> ok
        end,
    ok = atomvm:posix_close(RdFd),
    ok = atomvm:posix_close(WrFd),
    ok = atomvm:posix_unlink(Path).

% Test is based on the fact that `erlang:memory(binary)` count resources.
test_gc(HasSelect) ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    GCSubPid = spawn(fun() -> gc_loop(Path, undefined) end),
    MemorySize0 = erlang:memory(binary),
    call_gc_loop(GCSubPid, open),
    MemorySize1 = erlang:memory(binary),
    ?ASSERT_TRUE(MemorySize1 > MemorySize0),
    call_gc_loop(GCSubPid, close),
    call_gc_loop(GCSubPid, gc),
    MemorySize2 = erlang:memory(binary),
    ?ASSERT_EQUALS(MemorySize2, MemorySize0),

    call_gc_loop(GCSubPid, open),
    MemorySize3 = erlang:memory(binary),
    ?ASSERT_EQUALS(MemorySize3, MemorySize1),
    call_gc_loop(GCSubPid, open),
    call_gc_loop(GCSubPid, gc),
    MemorySize4 = erlang:memory(binary),
    ?ASSERT_EQUALS(MemorySize4, MemorySize1),
    call_gc_loop(GCSubPid, forget),
    call_gc_loop(GCSubPid, gc),
    MemorySize5 = erlang:memory(binary),
    ?ASSERT_EQUALS(MemorySize5, MemorySize0),

    if
        HasSelect ->
            call_gc_loop(GCSubPid, open),
            MemorySize6 = erlang:memory(binary),
            ?ASSERT_EQUALS(MemorySize6, MemorySize1),
            call_gc_loop(GCSubPid, select_write),
            call_gc_loop(GCSubPid, gc),
            MemorySize7 = erlang:memory(binary),
            ?ASSERT_EQUALS(MemorySize7, MemorySize1),
            call_gc_loop(GCSubPid, select_stop),
            call_gc_loop(GCSubPid, gc),
            MemorySize8 = erlang:memory(binary),
            ?ASSERT_EQUALS(MemorySize8, MemorySize1),
            call_gc_loop(GCSubPid, close),
            call_gc_loop(GCSubPid, gc),
            MemorySize9 = erlang:memory(binary),
            ?ASSERT_EQUALS(MemorySize9, MemorySize0);
        true ->
            ok
    end,

    call_gc_loop(GCSubPid, quit),
    ok = atomvm:posix_unlink(Path),
    ok.

call_gc_loop(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {Pid, Message} -> ok
    end.

gc_loop(Path, File) ->
    receive
        {select, _Resource, undefined, _Direction} ->
            gc_loop(Path, File);
        {Caller, open} ->
            {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
            Caller ! {self(), open},
            gc_loop(Path, Fd);
        {Caller, forget} ->
            Caller ! {self(), forget},
            gc_loop(Path, undefined);
        {Caller, gc} ->
            erlang:garbage_collect(),
            Caller ! {self(), gc},
            gc_loop(Path, File);
        {Caller, close} ->
            atomvm:posix_close(File),
            Caller ! {self(), close},
            gc_loop(Path, undefined);
        {Caller, select_write} ->
            ok = atomvm:posix_select_write(File, self(), undefined),
            Caller ! {self(), select_write},
            gc_loop(Path, File);
        {Caller, select_stop} ->
            ok = atomvm:posix_select_stop(File),
            Caller ! {self(), select_stop},
            gc_loop(Path, File);
        {Caller, quit} ->
            Caller ! {self(), quit}
    end.

% Test is based on the fact that `erlang:memory(binary)` count resources.
test_crash_no_leak(false) ->
    ok;
test_crash_no_leak(true) ->
    Before = erlang:memory(binary),
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    Pid = spawn(fun() -> crash_leak(Path) end),
    Ref = monitor(process, Pid),
    ok =
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 5000 -> timeout
        end,
    % Make sure sys_poll_events is called so selected resource is
    % properly disposed. This also gives time to context_destroy to
    % sweep mso after DOWN message is sent
    receive
    after 100 -> ok
    end,
    After = erlang:memory(binary),
    0 = After - Before,
    ok = atomvm:posix_unlink(Path),
    ok.

crash_leak(Path) ->
    ok = setup_and_forget(Path),
    % We don't really need to crash here
    % the test consists in not calling select_stop
    ok.

setup_and_forget(Path) ->
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    ok = atomvm:posix_select_write(Fd, self(), undefined),
    ok.

test_select_with_gone_process(false) ->
    ok;
test_select_with_gone_process(true) ->
    Before = erlang:memory(binary),
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    test_select_with_gone_process0(Path),
    % GC so Fd's ref count is decremented
    erlang:garbage_collect(),
    After = erlang:memory(binary),
    0 = After - Before,
    ok = atomvm:posix_unlink(Path),
    ok.

test_select_with_gone_process0(Path) ->
    GonePid = spawn(fun() -> ok end),
    Ref = monitor(process, GonePid),
    ok =
        receive
            {'DOWN', Ref, process, GonePid, _} -> ok
        after 5000 -> timeout
        end,
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    ok =
        try
            atomvm:posix_select_write(Fd, GonePid, undefined),
            fail
        catch
            error:badarg -> ok
        end,
    ok = atomvm:posix_close(Fd),
    ok.

test_select_with_listeners(false) ->
    ok;
test_select_with_listeners(_HasSelect) ->
    % Create listeners with a udp socket of the inet driver
    Port = open_port({spawn, "socket"}, []),
    ok =
        case
            port:call(
                Port,
                {init, [
                    {proto, udp},
                    {port, 0},
                    {controlling_process, self()},
                    {active, true},
                    {buffer, 128},
                    {timeout, infinity}
                ]}
            )
        of
            {error, noproc} -> {error, closed};
            out_of_memory -> {error, enomem};
            Result -> Result
        end,
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    ok = atomvm:posix_mkfifo(Path, 8#644),
    {ok, RdFd} = atomvm:posix_open(Path, [o_rdonly]),
    {ok, WrFd} = atomvm:posix_open(Path, [o_wronly]),
    SelectReadRef = make_ref(),
    ok = atomvm:posix_select_read(RdFd, self(), SelectReadRef),
    ok =
        receive
            {select, RdFd, SelectReadRef, _} -> fail
        after 200 -> ok
        end,
    {ok, 6} = atomvm:posix_write(WrFd, <<" World">>),
    ok =
        receive
            {select, RdFd, SelectReadRef, ready_input} -> ok;
            M2 -> {unexpected, M2}
        after 200 -> fail
        end,
    ok.

test_seek() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 5} = atomvm:posix_write(Fd, <<"Hello">>),
    {ok, 0} = atomvm:posix_seek(Fd, 0, seek_set),
    {ok, <<"Hello">>} = atomvm:posix_read(Fd, 10),
    {ok, 2} = atomvm:posix_seek(Fd, 2, seek_set),
    {ok, <<"llo">>} = atomvm:posix_read(Fd, 10),
    {ok, 5} = atomvm:posix_seek(Fd, 0, seek_end),
    eof = atomvm:posix_read(Fd, 10),
    {ok, 3} = atomvm:posix_seek(Fd, -2, seek_end),
    {ok, <<"lo">>} = atomvm:posix_read(Fd, 10),
    {ok, 0} = atomvm:posix_seek(Fd, 0, seek_set),
    {ok, 2} = atomvm:posix_seek(Fd, 2, seek_cur),
    {ok, <<"llo">>} = atomvm:posix_read(Fd, 10),
    ok = atomvm:posix_close(Fd),
    ok = atomvm:posix_unlink(Path).

test_pread_pwrite() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 5} = atomvm:posix_write(Fd, <<"Hello">>),
    {ok, <<"He">>} = atomvm:posix_pread(Fd, 2, 0),
    {ok, <<"llo">>} = atomvm:posix_pread(Fd, 10, 2),
    eof = atomvm:posix_pread(Fd, 10, 5),
    {ok, 6} = atomvm:posix_pwrite(Fd, <<" World">>, 5),
    {ok, <<"Hello World">>} = atomvm:posix_pread(Fd, 20, 0),
    %% Verify that pread/pwrite did not move the file cursor
    {ok, 5} = atomvm:posix_seek(Fd, 0, seek_cur),
    ok = atomvm:posix_close(Fd),
    ok = atomvm:posix_unlink(Path).

test_fsync() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 5} = atomvm:posix_write(Fd, <<"Hello">>),
    ok = atomvm:posix_fsync(Fd),
    ok = atomvm:posix_close(Fd),
    ok = atomvm:posix_unlink(Path).

test_ftruncate() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 11} = atomvm:posix_write(Fd, <<"Hello World">>),
    %% Truncate to shorter length
    ok = atomvm:posix_ftruncate(Fd, 5),
    {ok, 0} = atomvm:posix_seek(Fd, 0, seek_set),
    {ok, <<"Hello">>} = atomvm:posix_read(Fd, 20),
    eof = atomvm:posix_read(Fd, 1),
    %% Extend with null bytes
    ok = atomvm:posix_ftruncate(Fd, 8),
    {ok, 0} = atomvm:posix_seek(Fd, 0, seek_set),
    {ok, <<"Hello", 0, 0, 0>>} = atomvm:posix_read(Fd, 20),
    %% Truncate to zero
    ok = atomvm:posix_ftruncate(Fd, 0),
    {ok, 0} = atomvm:posix_seek(Fd, 0, seek_set),
    eof = atomvm:posix_read(Fd, 1),
    ok = atomvm:posix_close(Fd),
    ok = atomvm:posix_unlink(Path).

test_rename() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    NewPath = Path ++ ".renamed",
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 5} = atomvm:posix_write(Fd, <<"Hello">>),
    ok = atomvm:posix_close(Fd),
    %% Rename the file
    ok = atomvm:posix_rename(Path, NewPath),
    %% Old path should not exist
    {error, enoent} = atomvm:posix_open(Path, [o_rdonly]),
    %% New path should have the content
    {ok, Fd2} = atomvm:posix_open(NewPath, [o_rdonly]),
    {ok, <<"Hello">>} = atomvm:posix_read(Fd2, 10),
    ok = atomvm:posix_close(Fd2),
    %% Rename non-existent file should fail
    {error, enoent} = atomvm:posix_rename(Path, NewPath),
    ok = atomvm:posix_unlink(NewPath).

test_stat() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 11} = atomvm:posix_write(Fd, <<"Hello World">>),
    ok = atomvm:posix_close(Fd),
    {ok, #{
        st_size := 11,
        st_dev := Dev,
        st_ino := Ino,
        st_mode := Mode,
        st_nlink := Nlink,
        st_uid := Uid,
        st_gid := Gid,
        st_atime_s := Atime,
        st_mtime_s := Mtime,
        st_ctime_s := Ctime
    }} = atomvm:posix_stat(Path),
    true = is_integer(Dev),
    true = is_integer(Ino),
    true = is_integer(Mode),
    true = is_integer(Nlink),
    true = is_integer(Uid),
    true = is_integer(Gid),
    true = is_integer(Atime),
    true = is_integer(Mtime),
    true = is_integer(Ctime),
    {error, enoent} = atomvm:posix_stat(Path ++ ".nonexistent"),
    ok = atomvm:posix_unlink(Path).

test_fstat() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 11} = atomvm:posix_write(Fd, <<"Hello World">>),
    {ok, #{
        st_size := 11,
        st_dev := Dev,
        st_ino := Ino,
        st_mode := _Mode,
        st_nlink := _Nlink,
        st_uid := _Uid,
        st_gid := _Gid,
        st_atime_s := _Atime,
        st_mtime_s := _Mtime,
        st_ctime_s := _Ctime
    }} = atomvm:posix_fstat(Fd),
    %% stat and fstat should agree
    {ok, #{st_ino := Ino, st_dev := Dev}} = atomvm:posix_stat(Path),
    ok = atomvm:posix_close(Fd),
    ok = atomvm:posix_unlink(Path).

test_mkdir_rmdir() ->
    Base = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    Dir = Base ++ ".dir",
    ok = atomvm:posix_mkdir(Dir, 8#755),
    {ok, #{st_mode := _}} = atomvm:posix_stat(Dir),
    ok = atomvm:posix_rmdir(Dir),
    {error, enoent} = atomvm:posix_stat(Dir),
    ok = atomvm:posix_mkdir(Dir, 8#755),
    {error, eexist} = atomvm:posix_mkdir(Dir, 8#755),
    Path = Dir ++ "/file",
    {ok, Fd} = atomvm:posix_open(Path, [o_wronly, o_creat], 8#644),
    ok = atomvm:posix_close(Fd),
    {error, enotempty} = atomvm:posix_rmdir(Dir),
    ok = atomvm:posix_unlink(Path),
    ok = atomvm:posix_rmdir(Dir),
    {error, enoent} = atomvm:posix_rmdir(Dir),
    ok.

test_validation() ->
    Path = "/tmp/atomvm.tmp." ++ integer_to_list(erlang:system_time(millisecond)),
    {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat], 8#644),
    {ok, 5} = atomvm:posix_write(Fd, <<"Hello">>),
    ok = expect_badarg(fun() -> atomvm:posix_read(Fd, -1) end),
    ok = expect_badarg(fun() -> atomvm:posix_pread(Fd, -1, 0) end),
    ok = expect_badarg(fun() -> atomvm:posix_pread(Fd, 5, -1) end),
    ok = expect_badarg(fun() -> atomvm:posix_pwrite(Fd, <<"x">>, -1) end),
    ok = expect_badarg(fun() -> atomvm:posix_ftruncate(Fd, -1) end),
    {ok, 0} = atomvm:posix_seek(Fd, 0, seek_set),
    eof = atomvm:posix_pread(Fd, 0, 0),
    ok = atomvm:posix_close(Fd),
    ok = atomvm:posix_unlink(Path).

expect_badarg(Fun) ->
    try
        Fun(),
        {error, no_exception}
    catch
        error:badarg -> ok
    end.

test_subprocess(false) ->
    ok;
test_subprocess(true) ->
    ok = test_subprocess_echo(),
    ok = test_subprocess_env(),
    ok.

test_subprocess_echo() ->
    {ok, _Pid, StdoutFd} = atomvm:subprocess("/bin/echo", ["echo"], undefined, [stdout]),
    {ok, <<"\n">>} = atomvm:posix_read(StdoutFd, 10),
    eof = atomvm:posix_read(StdoutFd, 10),
    ok = atomvm:posix_close(StdoutFd),
    ok.

test_subprocess_env() ->
    {ok, _Pid, StdoutFd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "echo $FOO"], ["FOO=bar"], [
        stdout
    ]),
    {ok, <<"bar\n">>} = atomvm:posix_read(StdoutFd, 10),
    eof = atomvm:posix_read(StdoutFd, 10),
    ok = atomvm:posix_close(StdoutFd),
    ok.
