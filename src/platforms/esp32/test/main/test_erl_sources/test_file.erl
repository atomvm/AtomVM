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

-export([start/0]).

start() ->
    ok = test_basic_file(),
    ok = test_gc(),
    ok.

% esp-idf limitations:
% filenames should be up to 8+3
% O_CREAT | O_WRONLY doesn't work in v4.4.4 (https://github.com/espressif/esp-idf/issues/1817)
% files are not selectable

test_basic_file() ->
    Path = "/sdcard/atomvm-1.txt",
    {ok, Fd} = atomvm:posix_open(Path, [o_wronly, o_creat, o_excl], 8#644),
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

% Test is based on the fact that `erlang:memory(binary)` count resources.
test_gc() ->
    Path = "/sdcard/atomvm-2.txt",
    GCSubPid = spawn(fun() -> gc_loop(Path, undefined) end),
    MemorySize0 = erlang:memory(binary),
    call_gc_loop(GCSubPid, open),
    MemorySize1 = erlang:memory(binary),
    true = MemorySize1 > MemorySize0,
    call_gc_loop(GCSubPid, close),
    call_gc_loop(GCSubPid, gc),
    MemorySize2 = erlang:memory(binary),
    true = MemorySize2 =:= MemorySize0,

    call_gc_loop(GCSubPid, open),
    MemorySize3 = erlang:memory(binary),
    true = MemorySize3 =:= MemorySize1,
    call_gc_loop(GCSubPid, open),
    call_gc_loop(GCSubPid, gc),
    MemorySize4 = erlang:memory(binary),
    true = MemorySize4 =:= MemorySize1,
    call_gc_loop(GCSubPid, forget),
    call_gc_loop(GCSubPid, gc),
    MemorySize5 = erlang:memory(binary),
    true = MemorySize5 =:= MemorySize0,

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
        {Caller, open} ->
            {ok, Fd} = atomvm:posix_open(Path, [o_rdwr, o_creat, o_append], 8#644),
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
        {Caller, quit} ->
            Caller ! {self(), quit}
    end.
