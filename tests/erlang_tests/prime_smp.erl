%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

-module(prime_smp).

-export([start/0, calculate_primes/0]).

start() ->
    ok =
        case erlang:system_info(schedulers) of
            1 ->
                ok;
            N when is_integer(N) ->
                benchmark_smp(N)
        end,
    0.

benchmark_smp(Cores) ->
    benchmark_smp(Cores, 0).

benchmark_smp(_Cores, Attempts) when
    Attempts > 10
->
    failed;
benchmark_smp(Cores, Attempts) ->
    erlang:system_flag(schedulers_online, Cores),
    SMP = measure_time(),
    erlang:system_flag(schedulers_online, 1),
    SingleCore = measure_time(),
    if
        SingleCore > SMP + (SMP div 2) ->
            ok;
        true ->
            erlang:display({SingleCore, SMP, Attempts}),
            benchmark_smp(Cores, Attempts + 1)
    end.

measure_time() ->
    {Pid1, Ref1} = spawn_opt(?MODULE, calculate_primes, [], [monitor]),
    {Pid2, Ref2} = spawn_opt(?MODULE, calculate_primes, [], [monitor]),
    {Pid3, Ref3} = spawn_opt(?MODULE, calculate_primes, [], [monitor]),
    Start = erlang:system_time(microsecond),
    Pid1 ! {self(), start},
    Pid2 ! {self(), start},
    Pid3 ! {self(), start},
    receive
        {'DOWN', Ref1, process, Pid1, normal} -> ok
    end,
    receive
        {'DOWN', Ref2, process, Pid2, normal} -> ok
    end,
    receive
        {'DOWN', Ref3, process, Pid3, normal} -> ok
    end,
    End = erlang:system_time(microsecond),
    End - Start.

is_prime(Num) ->
    test_prime(Num, 2).

test_prime(Num, I) when Num == I ->
    true;
test_prime(Num, I) ->
    if
        Num rem I == 0 ->
            false;
        true ->
            test_prime(Num, I + 1)
    end.

calculate_primes() ->
    receive
        {_Parent, start} -> ok
    end,
    calculate_list(1, 2000).

calculate_list(First, Last) when First < 2 ->
    calculate_list(First + 1, Last);
calculate_list(First, Last) when First == Last ->
    case is_prime(Last) of
        true ->
            Last;
        false ->
            []
    end;
calculate_list(First, Last) ->
    case is_prime(First) of
        true ->
            [First | calculate_list(First + 1, Last)];
        false ->
            calculate_list(First + 1, Last)
    end.
