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

-module(test_throw_call_ext_last).

-export([start/0, loop/1]).

-record(state, {
    bin
}).

start() ->
    ok = run_test(fun() -> test_count_binary() end),
    {error, {heap_delta, _Delta}} = run_test(fun() -> test_spawn_fun_sub_binary() end),
    0.

test_spawn_fun_sub_binary() ->
    Bin = create_binary(1024),
    BinarySize = erlang:byte_size(Bin),
    %%
    %% Spawn a function, passing a refc binary through the args
    %%
    LargeSubBin = binary:part(Bin, 1, BinarySize - 1),
    Pid = spawn_opt(fun() -> loop(#state{bin = LargeSubBin}) end, []),
    PidHeapSize0 = get_heap_size(Pid),
    %%
    %% Make sure we can get what we spawned
    %%
    LargeSubBin = send(Pid, get),
    %%
    %% Free the refc binary; heap should decrease
    %%
    ok = send(Pid, free),
    PidHeapSize2 = get_heap_size(Pid),
    case PidHeapSize2 - PidHeapSize0 of
        0 -> ok;
        % should be call_ext_last
        Delta -> throw({heap_delta, Delta})
    end,
    ok = send(Pid, halt),
    ok.

test_count_binary() ->
    _ = create_binary(1024),
    ok.

%%
%% helper functions
%%

get_heap_size() ->
    erlang:garbage_collect(),
    {heap_size, Size} = erlang:process_info(self(), heap_size),
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
    S = create_string(N, []),
    R = erlang:list_to_binary(S),
    R;
create_binary(S) when is_list(S) ->
    list_to_binary(S).

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
    Pid ! Result.
