%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

-module(test_sub_binaries).

-export([start/0, loop/1]).

-define(LITERAL_BIN, <<
    "01234567890123456789012345678901234567890123456789"
    "01234567890123456789012345678901234567890123456789"
    "01234567890123456789012345678901234567890123456789"
    "01234567890123456789012345678901234567890123456789"
    "01234567890123456789012345678901234567890123456789"
    "01234567890123456789012345678901234567890123456789"
    "01234567890123456789012345678901234567890123456789"
    "01234567890123456789012345678901234567890123456789"
>>).

-record(state, {
    bin
}).

-define(VERIFY(X),
    case X of
        true -> ok;
        _ -> throw({assertion_failure, ??X, ?MODULE, ?FUNCTION_NAME, ?LINE})
    end
).

start() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            ok = test_common();
        _ ->
            ok = test_common(),
            ok = test_atom()
    end,
    0.

test_common() ->
    ok = run_test(fun() -> test_count_binary() end),
    ok = run_test(fun() -> test_sub_sub_binary() end),
    ok.

test_atom() ->
    % Most of these tests rely on a property of heap size that isn't
    % verified with BEAM
    ok = run_test(fun() -> test_heap_sub_binary() end),
    ok = run_test(fun() -> test_const_sub_binary() end),
    ok = run_test(fun() -> test_non_const_sub_binary() end),
    ok = run_test(fun() -> test_send_sub_binary() end),
    ok = run_test(fun() -> test_spawn_sub_binary() end),
    ok = run_test(fun() -> test_spawn_fun_sub_binary() end),
    ok = run_test(fun() -> test_split_sub_binary() end),
    ok = run_test(fun() -> test_bit_syntax_tail() end),
    ok = run_test(fun() -> test_bit_syntax_get_binary() end),
    ok.

test_sub_sub_binary() ->
    Bin = create_binary(get_largest_heap_binary_size()),

    SubBin = binary:part(Bin, 1, 7),
    <<2, 3, 4, 5, 6, 7, 8>> = SubBin,
    <<4, 5, 6, 7, 8>> = binary:part(SubBin, 2, 5),
    ok.

test_heap_sub_binary() ->
    MemoryBinarySize = erlang:memory(binary),
    HeapSize0 = get_heap_size(),
    Bin = create_binary(get_largest_heap_binary_size()),
    ?VERIFY(MemoryBinarySize == erlang:memory(binary)),
    HeapSize1 = get_heap_size(),
    ?VERIFY(HeapSize0 < HeapSize1),

    SubBin = binary:part(Bin, 1, 7),
    HeapSize2 = get_heap_size(),
    ?VERIFY(HeapSize1 < HeapSize2),
    ?VERIFY((HeapSize2 - HeapSize1) >= 8),
    ?VERIFY(MemoryBinarySize == erlang:memory(binary)),

    id(Bin),
    id(SubBin),
    ok.

test_const_sub_binary() ->
    MemoryBinarySize = erlang:memory(binary),
    HeapSize0 = get_heap_size(),
    BinarySize = erlang:byte_size(?LITERAL_BIN),
    ?VERIFY(HeapSize0 < BinarySize),
    ?VERIFY(MemoryBinarySize == erlang:memory(binary)),

    SmallSubBin = binary:part(?LITERAL_BIN, 1, 15),
    HeapSize1 = get_heap_size(),
    ?VERIFY(HeapSize0 < HeapSize1),
    ?VERIFY(erlang:byte_size(SmallSubBin) < HeapSize1),

    LargeSubBin = binary:part(?LITERAL_BIN, 1, BinarySize - 1),
    HeapSize2 = get_heap_size(),
    ?VERIFY((HeapSize2 - HeapSize1) >= 8),
    ?VERIFY((HeapSize2 - HeapSize1) < BinarySize div 4),
    ?VERIFY(MemoryBinarySize == erlang:memory(binary)),

    SubSubBin = binary:part(LargeSubBin, 0, BinarySize - 2),
    HeapSize3 = get_heap_size(),
    ?VERIFY((HeapSize3 - HeapSize2) >= 8),
    ?VERIFY((HeapSize3 - HeapSize2) < BinarySize div 4),
    ?VERIFY(MemoryBinarySize == erlang:memory(binary)),

    id(SmallSubBin),
    id(LargeSubBin),
    id(SubSubBin),
    ok.

test_non_const_sub_binary() ->
    MemoryBinarySize = erlang:memory(binary),
    HeapSize0 = get_heap_size(),
    String = create_string(1024),
    HeapSize1 = get_heap_size(),
    ?VERIFY(HeapSize0 < HeapSize1),
    Bin = create_binary(String),
    BinarySize = erlang:byte_size(Bin),
    HeapSize2 = get_heap_size(),
    ?VERIFY(HeapSize1 < HeapSize2),
    ?VERIFY(HeapSize2 < (HeapSize1 + erlang:byte_size(Bin))),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),

    LargeSubBin = binary:part(Bin, 1, BinarySize - 1),
    HeapSize3 = get_heap_size(),
    ?VERIFY((HeapSize3 - HeapSize2) < BinarySize div 4),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),

    id(String),
    id(Bin),
    id(LargeSubBin),
    ok.

test_send_sub_binary() ->
    MemoryBinarySize = erlang:memory(binary),
    Bin = create_binary(1024),
    BinarySize = erlang:byte_size(Bin),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    Pid = spawn_opt(fun() -> loop(#state{}) end, []),
    PidHeapSize0 = get_heap_size(Pid),
    %%
    %% Send the process a refc binary, and check heap size
    %%
    LargeSubBin = binary:part(Bin, 1, BinarySize - 1),
    ok = send(Pid, {ref, LargeSubBin}),
    PidHeapSize1 = get_heap_size(Pid),
    ?VERIFY(PidHeapSize0 < PidHeapSize1),
    ?VERIFY(PidHeapSize1 < 1024),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    %%
    %% Make sure we can get what we sent
    %%
    LargeSubBin = send(Pid, get),
    %%
    %% Free the refc binary; heap should decrease
    %%
    ok = send(Pid, free),
    PidHeapSize2 = get_heap_size(Pid),
    ?VERIFY(PidHeapSize2 < PidHeapSize1),
    ok = send(Pid, halt),
    ok.

test_spawn_sub_binary() ->
    MemoryBinarySize = erlang:memory(binary),
    Bin = create_binary(1024),
    BinarySize = erlang:byte_size(Bin),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    %%
    %% Spawn a function, passing a refc binary through the args
    %%
    LargeSubBin = binary:part(Bin, 1, BinarySize - 1),
    Pid = spawn_opt(?MODULE, loop, [#state{bin = LargeSubBin}], []),
    PidHeapSize0 = get_heap_size(Pid),
    %%
    %% Make sure we can get what we spawned
    %%
    LargeSubBin = send(Pid, get),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    %%
    %% Free the refc binary; heap should decrease
    %%
    ok = send(Pid, free),
    PidHeapSize2 = get_heap_size(Pid),
    ?VERIFY(PidHeapSize2 < PidHeapSize0),
    ok = send(Pid, halt),
    ok.

test_spawn_fun_sub_binary() ->
    MemoryBinarySize = erlang:memory(binary),
    Bin = create_binary(1024),
    BinarySize = erlang:byte_size(Bin),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    %%
    %% Spawn a function, passing a refc binary through the args
    %%
    LargeSubBin = binary:part(Bin, 1, BinarySize - 1),
    Pid = spawn_opt(fun() -> loop(#state{bin = LargeSubBin}) end, []),
    PidHeapSize0 = get_heap_size(Pid),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    %%
    %% Make sure we can get what we spawned
    %%
    LargeSubBin = send(Pid, get),
    %%
    %% Free the refc binary; heap should decrease
    %%
    ok = send(Pid, free),
    PidHeapSize2 = get_heap_size(Pid),
    ?VERIFY(PidHeapSize2 < PidHeapSize0),
    ok = send(Pid, halt),
    ok.

test_split_sub_binary() ->
    MemoryBinarySize = erlang:memory(binary),
    HeapSize0 = get_heap_size(),
    Bin = create_binary(1024),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    [SmallSubBin, LargeSubBin] = binary:split(Bin, <<4, 5, 6>>),
    HeapSize1 = get_heap_size(),

    ?VERIFY(erlang:byte_size(SmallSubBin) < (HeapSize1 - HeapSize0)),
    ?VERIFY((HeapSize1 - HeapSize0) < erlang:byte_size(LargeSubBin)),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),

    id(SmallSubBin),
    id(LargeSubBin),
    ok.

test_bit_syntax_tail() ->
    MemoryBinarySize = erlang:memory(binary),
    _HeapSize0 = get_heap_size(),
    Bin = create_binary(1024),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),
    BinarySize = erlang:byte_size(Bin),
    HeapSize1 = get_heap_size(),
    LargeSubBin = binary:part(Bin, 1, BinarySize - 1),

    LargeSubBin = match_rest(Bin),
    HeapSize2 = get_heap_size(),

    ?VERIFY((HeapSize2 - HeapSize1) < erlang:byte_size(LargeSubBin)),
    ?VERIFY(MemoryBinarySize + 1024 == erlang:memory(binary)),

    id(Bin),
    id(LargeSubBin),
    ok.

match_rest(<<_:8, Rest/binary>>) ->
    Rest.

test_bit_syntax_get_binary() ->
    _HeapSize0 = get_heap_size(),
    Bin = create_binary(1024),
    HeapSize1 = get_heap_size(),
    LargeSubBin = binary:part(Bin, 0, 512),

    LargeSubBin = match_binary(Bin),
    HeapSize2 = get_heap_size(),

    ?VERIFY((HeapSize2 - HeapSize1) < erlang:byte_size(LargeSubBin)),

    id(Bin),
    id(LargeSubBin),
    ok.

match_binary(<<Initial:512/binary, _Rest/binary>>) ->
    Initial.

test_count_binary() ->
    Bin = create_binary(1024),
    ?VERIFY(1024 =:= count_binary(Bin, 0)),
    ok.

count_binary(<<"">>, Accum) ->
    Accum;
count_binary(<<_Byte:8, Rest/binary>>, Accum) ->
    count_binary(Rest, Accum + 1).

%%
%% helper functions
%%

get_largest_heap_binary_size() ->
    case erlang:system_info(wordsize) of
        4 ->
            31;
        8 ->
            63
    end.

get_heap_size() ->
    erlang:garbage_collect(),
    {total_heap_size, Size} = erlang:process_info(self(), total_heap_size),
    Size * erlang:system_info(wordsize).

get_heap_size(Pid) ->
    send(Pid, get_heap_size).

send(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, Msg},
    receive
        {Ref, Reply} -> Reply
    end.

loop(State) ->
    erlang:garbage_collect(),
    receive
        {Pid, Ref, get} ->
            Pid ! {Ref, State#state.bin},
            loop(State);
        {Pid, Ref, free} ->
            Pid ! {Ref, ok},
            loop(State#state{bin = undefined});
        {Pid, Ref, get_heap_size} ->
            Pid ! {Ref, get_heap_size()},
            loop(State);
        {Pid, Ref, {ref, Bin}} ->
            Pid ! {Ref, ok},
            loop(State#state{bin = Bin});
        {Pid, Ref, halt} ->
            Pid ! {Ref, ok}
    end.

create_binary(N) when is_integer(N) ->
    erlang:list_to_binary(create_string(N, []));
create_binary(S) when is_list(S) ->
    list_to_binary(S).

create_string(N) ->
    create_string(N, []).

create_string(0, Accum) ->
    Accum;
create_string(N, Accum) ->
    create_string(N - 1, [N rem 256 | Accum]).

run_test(Fun) ->
    Self = self(),
    _Pid = spawn_opt(fun() -> execute(Self, Fun) end, []),
    receive
        ok ->
            ok;
        Error ->
            Error
    end.

execute(Pid, Fun) ->
    Result =
        try
            Fun(),
            ok
        catch
            _:Error ->
                {error, Error}
        end,
    erlang:garbage_collect(),
    Pid ! Result.

id(X) -> X.
