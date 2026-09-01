%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% Copyright 2026 Peter M. <petermm@gmail.com>
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

%%-----------------------------------------------------------------------------
%% @doc UART implementation for RTEMS using Termios device nodes.
%%
%% This module implements the {@link uart_hal} behaviour. The peripheral name
%% is a device path such as `"/dev/console"', `"/dev/console_a"' (erc32) or
%% `"/dev/ttyS0"' (imx7).
%%
%% Example:
%% ```
%% UART = uart:open([{peripheral, "/dev/console"}, {speed, 115200}]),
%% ok = uart:write(UART, <<"hello">>),
%% ok = uart:close(UART).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(uart).

-behaviour(uart_hal).

-export([open/1, open/2, close/1, read/1, read/2, write/2]).
-export([init/1, deinit/1, write/3, read/3]).
-export([write_nif/2, read_nif/2]).

-define(DEFAULT_PERIPHERAL, "/dev/console").
-define(DEFAULT_SPEED, 115200).
-define(DEFAULT_DATA_BITS, 8).
-define(DEFAULT_STOP_BITS, 1).
-define(DEFAULT_PARITY, none).
-define(DEFAULT_TIMEOUT_MS, 5000).
-define(POLL_INTERVAL_MS, 10).

-type uart_resource() :: reference().
-type uart() :: pid().

-export_type([uart/0, uart_resource/0]).

%%-----------------------------------------------------------------------------
%% High-level API (uart_hal behaviour)
%%-----------------------------------------------------------------------------

-spec open(Opts :: [{atom(), term()}]) -> uart().
open(Opts) ->
    Config = parse_opts(Opts),
    case ?MODULE:init(Config) of
        {ok, Resource} ->
            spawn_link(fun() -> loop(Resource) end);
        {error, Reason} ->
            error(Reason)
    end.

-spec open(Name :: string() | binary(), Opts :: [{atom(), term()}]) -> uart().
open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

-spec close(UART :: uart()) -> ok | {error, term()}.
close(Pid) when is_pid(Pid) ->
    call(Pid, close).

-spec read(UART :: uart()) -> {ok, binary()} | {error, term()}.
read(Pid) when is_pid(Pid) ->
    call(Pid, read).

-spec read(UART :: uart(), Timeout :: pos_integer()) -> {ok, binary()} | {error, term()}.
read(Pid, Timeout) when is_pid(Pid), is_integer(Timeout), Timeout > 0 ->
    call(Pid, {read, Timeout}).

-spec write(UART :: uart(), Data :: iodata()) -> ok | {error, term()}.
write(Pid, Data) when is_pid(Pid) ->
    call(Pid, {write, Data, ?DEFAULT_TIMEOUT_MS}).

%%-----------------------------------------------------------------------------
%% Low-level NIFs
%%-----------------------------------------------------------------------------

-spec init(Config :: [{atom(), term()}]) -> {ok, uart_resource()} | {error, term()}.
init(_Config) ->
    erlang:nif_error(undefined).

-spec deinit(Resource :: uart_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

-spec write(Resource :: uart_resource(), Data :: binary(), TimeoutMs :: timeout()) ->
    non_neg_integer() | {error, term()}.
write(Resource, Data, TimeoutMs) when is_binary(Data) ->
    write_loop(Resource, Data, 0, timeout_deadline(TimeoutMs));
write(_Resource, _Data, _TimeoutMs) ->
    erlang:error(badarg).

-spec write_nif(Resource :: uart_resource(), Data :: binary()) ->
    non_neg_integer() | {error, term()}.
write_nif(_Resource, _Data) ->
    erlang:nif_error(undefined).

-spec read(Resource :: uart_resource(), Count :: non_neg_integer(), TimeoutMs :: timeout()) ->
    {ok, binary()} | {error, term()}.
read(Resource, Count, TimeoutMs) when is_integer(Count), Count >= 0 ->
    read_loop(Resource, Count, timeout_deadline(TimeoutMs));
read(_Resource, _Count, _TimeoutMs) ->
    erlang:error(badarg).

-spec read_nif(Resource :: uart_resource(), Count :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
read_nif(_Resource, _Count) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% Internal helpers
%%-----------------------------------------------------------------------------

call(Pid, Request) ->
    MRef = erlang:monitor(process, Pid),
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, {server_died, Reason}}
    end.

loop(Resource) ->
    receive
        {From, Ref, Request} ->
            case handle_request(Resource, Request) of
                {reply, Reply, stop} ->
                    From ! {Ref, Reply};
                {reply, Reply} ->
                    From ! {Ref, Reply},
                    loop(Resource)
            end
    end.

handle_request(Resource, close) ->
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(Resource, read) ->
    case ?MODULE:read(Resource, 1, 0) of
        {ok, Byte} ->
            {reply, {ok, read_available(Resource, [Byte])}};
        {error, _} = Error ->
            {reply, Error}
    end;
handle_request(Resource, {read, Timeout}) ->
    case ?MODULE:read(Resource, 1, Timeout) of
        {ok, Byte} ->
            {reply, {ok, read_available(Resource, [Byte])}};
        {error, _} = Error ->
            {reply, Error}
    end;
handle_request(Resource, {write, Data, Timeout}) ->
    Bin = iolist_to_binary(Data),
    case ?MODULE:write(Resource, Bin, Timeout) of
        N when is_integer(N) -> {reply, ok};
        {error, _} = Error -> {reply, Error}
    end.

read_available(Resource, Acc) ->
    case ?MODULE:read(Resource, 1, 0) of
        {ok, Byte} ->
            read_available(Resource, [Byte | Acc]);
        {error, timeout} ->
            erlang:iolist_to_binary(lists:reverse(Acc));
        {error, _} ->
            erlang:iolist_to_binary(lists:reverse(Acc))
    end.

write_loop(_Resource, <<>>, Written, _Deadline) ->
    Written;
write_loop(Resource, Data, Written, Deadline) ->
    case ?MODULE:write_nif(Resource, Data) of
        N when is_integer(N), N > 0 ->
            <<_:N/binary, Rest/binary>> = Data,
            continue_write(Resource, Rest, Written + N, Deadline);
        0 ->
            retry_write(Resource, Data, Written, Deadline);
        {error, timeout} ->
            retry_write(Resource, Data, Written, Deadline);
        {error, _} = Error ->
            Error
    end.

continue_write(_Resource, <<>>, Written, _Deadline) ->
    Written;
continue_write(Resource, Data, Written, infinity) ->
    write_loop(Resource, Data, Written, infinity);
continue_write(Resource, Data, Written, Deadline) ->
    case erlang:monotonic_time(millisecond) >= Deadline of
        true -> {error, timeout};
        false -> write_loop(Resource, Data, Written, Deadline)
    end.

retry_write(Resource, Data, Written, Deadline) ->
    case wait_to_retry(Deadline) of
        ok -> write_loop(Resource, Data, Written, Deadline);
        timeout -> {error, timeout}
    end.

read_loop(Resource, Count, Deadline) ->
    case ?MODULE:read_nif(Resource, Count) of
        {error, timeout} ->
            case wait_to_retry(Deadline) of
                ok -> read_loop(Resource, Count, Deadline);
                timeout -> {error, timeout}
            end;
        Result ->
            Result
    end.

timeout_deadline(infinity) ->
    infinity;
timeout_deadline(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    erlang:monotonic_time(millisecond) + TimeoutMs;
timeout_deadline(_TimeoutMs) ->
    erlang:error(badarg).

wait_to_retry(infinity) ->
    timer:sleep(?POLL_INTERVAL_MS),
    ok;
wait_to_retry(Deadline) ->
    RemainingMs = Deadline - erlang:monotonic_time(millisecond),
    case RemainingMs =< 0 of
        true ->
            timeout;
        false ->
            timer:sleep(poll_interval(RemainingMs)),
            ok
    end.

poll_interval(RemainingMs) when RemainingMs < ?POLL_INTERVAL_MS ->
    RemainingMs;
poll_interval(_RemainingMs) ->
    ?POLL_INTERVAL_MS.

parse_opts(Opts) ->
    Peripheral = proplists:get_value(peripheral, Opts, ?DEFAULT_PERIPHERAL),
    Speed = proplists:get_value(speed, Opts, ?DEFAULT_SPEED),
    DataBits = proplists:get_value(data_bits, Opts, ?DEFAULT_DATA_BITS),
    StopBits = proplists:get_value(stop_bits, Opts, ?DEFAULT_STOP_BITS),
    Parity = proplists:get_value(parity, Opts, ?DEFAULT_PARITY),
    [
        {peripheral, Peripheral},
        {speed, Speed},
        {data_bits, DataBits},
        {stop_bits, StopBits},
        {parity, parity_to_int(Parity)}
    ].

parity_to_int(none) -> 0;
parity_to_int(odd) -> 1;
parity_to_int(even) -> 2.
