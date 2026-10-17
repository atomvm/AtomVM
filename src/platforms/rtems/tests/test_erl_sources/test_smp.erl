%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_smp).
-export([start/0]).

start() ->
    erlang:display({test_smp_start, atomvm:platform()}),
    Schedulers = erlang:system_info(schedulers),
    SchedulersOnline = erlang:system_info(schedulers_online),
    erlang:display({schedulers, Schedulers, schedulers_online, SchedulersOnline}),
    ok = timer:sleep(50),
    erlang:display({timer_sleep, ok}),
    true = is_integer(Schedulers) andalso Schedulers >= 1,
    true = is_integer(SchedulersOnline) andalso SchedulersOnline >= 1,

    % Concurrency & messaging smoke test across schedulers
    Self = self(),
    NumWorkers = 8,
    WorkerPids = [spawn(fun() -> worker_loop(Self, I) end) || I <- lists:seq(1, NumWorkers)],
    [Pid ! {ping, Self} || Pid <- WorkerPids],
    ok = collect_pongs(NumWorkers),
    erlang:display({smp_ping_pong, ok}),

    % Parallel computation stress test
    ComputePids = [spawn(fun() -> compute_work(Self, I, 500) end) || I <- lists:seq(1, NumWorkers)],
    ok = collect_results(NumWorkers, 0),
    erlang:display({smp_parallel_compute, ok}),

    erlang:display({test_smp_done, ok}),
    ok.

worker_loop(Parent, Id) ->
    receive
        {ping, Parent} ->
            Parent ! {pong, Id, self()}
    end.

collect_pongs(0) -> ok;
collect_pongs(N) when N > 0 ->
    receive
        {pong, _Id, _Pid} ->
            collect_pongs(N - 1)
    after 5000 ->
        erlang:error(pong_timeout)
    end.

compute_work(Parent, Id, Iterations) ->
    Sum = lists:foldl(fun(X, Acc) -> Acc + X end, 0, lists:seq(1, Iterations)),
    Parent ! {result, Id, Sum}.

collect_results(0, _Total) -> ok;
collect_results(N, Total) when N > 0 ->
    receive
        {result, _Id, Sum} ->
            collect_results(N - 1, Total + Sum)
    after 5000 ->
        erlang:error(result_timeout)
    end.
