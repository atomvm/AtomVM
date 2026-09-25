%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%%
%% Stress test for the generational (minor) GC running while message
%% fragments are present in the heap.
%%
%% Receiving a message appends its heap fragment to the process heap
%% (heap.root->next != NULL) via memory_heap_append_fragment. That path keeps
%% the mature region inside the root fragment, so a minor GC can collect it.
%%
%% fullsweep_after is forced low so the collector cycles minor -> full -> minor
%% repeatedly rather than only ever running minor GCs.
%%
-module(test_minor_gc_messages).

-export([start/0]).

%% Each iteration round-trips a payload through the echo process and churns
%% young allocations, triggering several GCs
-define(ITERATIONS, 50).
-define(SURVIVOR_WINDOW, 16).
%% Checksums are reduced modulo this prime so the accumulator stays a small
%% integer
-define(CK_MOD, 1000000007).

start() ->
    {Pid, Ref} = spawn_opt(fun worker/0, [monitor, {fullsweep_after, 8}]),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            0;
        {'DOWN', Ref, process, Pid, Reason} ->
            {fail, Reason};
        Other ->
            {unexpected, Other}
    after 300000 ->
        timeout
    end.

worker() ->
    Echo = spawn_opt(fun echo/0, []),
    Mature = make_mature(200),
    MSum = checksum(Mature),
    %% Sanity: two independent computations of the mature checksum agree.
    MSum = checksum(Mature),
    ok = run(?ITERATIONS, Echo, Mature, MSum, []),
    %% The mature structure must still be intact after the whole run.
    MSum = checksum(Mature),
    Echo ! stop,
    ok.

run(0, _Echo, _Mature, _MSum, _Survivors) ->
    ok;
run(N, Echo, Mature, MSum, Survivors) ->
    Payload = make_payload(N),
    PSum = checksum(Payload),
    %% Round-trip the payload through another process: the reply arrives as a
    %% freshly malloc'd message fragment appended to our heap.
    Echo ! {self(), Payload},
    Got =
        receive
            {reply, R} -> R
        after 5000 ->
            error(reply_timeout)
        end,
    %% Got now lives in the appended fragment. Verify it copied across intact,
    %% verify the mature structure, then churn young allocations to force a GC
    %% while the fragment is still referenced (root->next != NULL).
    PSum = checksum(Got),
    %% Verifying the whole mature structure every iteration is O(mature size);
    %% do it periodically (and once more after the loop) to keep the run fast
    %% while still catching old-generation corruption.
    ok = maybe_check_mature(N, Mature, MSum),
    _Garbage = make_garbage(32),
    %% Keep a sliding window of received payloads alive so message-origin terms
    %% are promoted into the old generation by a minor GC, then re-verify them.
    %% The expected checksum is stored alongside (Got is the echoed Payload, so
    %% its checksum is PSum) to avoid rebuilding payloads on every iteration.
    Survivors2 = take(?SURVIVOR_WINDOW, [{Got, PSum} | Survivors]),
    ok = verify_survivors(Survivors2),
    run(N - 1, Echo, Mature, MSum, Survivors2).

maybe_check_mature(N, Mature, MSum) when N rem 16 =:= 0 ->
    MSum = checksum(Mature),
    ok;
maybe_check_mature(_N, _Mature, _MSum) ->
    ok.

echo() ->
    receive
        {From, Payload} ->
            From ! {reply, Payload},
            echo();
        stop ->
            ok
    end.

%% A stable structure kept referenced for the whole run; becomes old-gen data.
make_mature(0) ->
    [];
make_mature(N) ->
    [{N, N * N, make_binary(48, N)} | make_mature(N - 1)].

%% Each payload mixes small ints, a boxed bignum and a refc-sized binary.
make_payload(N) ->
    Ints = make_ints(8, N),
    Big = (N + 1) * 1000000007 * 1000000007,
    Bin = make_binary(100, N),
    {payload, N, Ints, Big, Bin}.

make_ints(0, _N) ->
    [];
make_ints(K, N) ->
    [(N * K) rem 1000003 | make_ints(K - 1, N)].

%% Transient young garbage to create heap pressure (40-byte heap binaries).
make_garbage(0) ->
    [];
make_garbage(N) ->
    [{N, make_binary(40, N)} | make_garbage(N - 1)].

make_binary(Len, N) ->
    list_to_binary(make_bytes(Len, N)).

make_bytes(0, _N) ->
    [];
make_bytes(I, N) ->
    [(N + I) rem 256 | make_bytes(I - 1, N)].

verify_survivors([]) ->
    ok;
verify_survivors([{Data, Expected} | T]) ->
    case checksum(Data) of
        Expected -> verify_survivors(T);
        Other -> error({survivor_mismatch, Expected, Other})
    end.

take(0, _) ->
    [];
take(_, []) ->
    [];
take(N, [H | T]) ->
    [H | take(N - 1, T)].

%% Deterministic structural checksum over the term shapes used above.
checksum(T) when is_integer(T) ->
    T;
checksum(T) when is_atom(T) ->
    checksum(atom_to_list(T));
checksum(T) when is_binary(T) ->
    bin_sum(T, 0);
checksum(T) when is_list(T) ->
    list_sum(T, 7);
checksum(T) when is_tuple(T) ->
    checksum(tuple_to_list(T)).

list_sum([], Acc) ->
    Acc;
list_sum([H | T], Acc) ->
    list_sum(T, (Acc * 31 + checksum(H)) rem ?CK_MOD).

bin_sum(<<>>, Acc) ->
    Acc;
bin_sum(<<B, Rest/binary>>, Acc) ->
    bin_sum(Rest, (Acc * 31 + B) rem ?CK_MOD).
