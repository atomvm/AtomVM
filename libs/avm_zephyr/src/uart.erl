%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc AtomVM UART interface for Zephyr.
%%
%% Zephyr owns UART pin routing through devicetree. Applications select a
%% controller with `{peripheral, Index | Name}' and may request runtime line
%% settings on drivers that support `CONFIG_UART_USE_RUNTIME_CONFIGURE'.
-module(uart).

-behaviour(uart_hal).

-export([open/1, open/2, close/1, read/1, read/2, write/2]).
-export([init/1, deinit/1, write/3, read/3, abort/1, get_state/1, get_error/1]).

-type peripheral() :: non_neg_integer() | string() | binary().
-type option() ::
    {peripheral, peripheral()}
    | {speed, pos_integer()}
    | {data_bits, 5..9}
    | {stop_bits, 1 | 2}
    | {parity, none | odd | even}
    | {flow_control, none | hardware}.
-type uart_resource() :: reference().
-type uart() :: pid().
-type uart_state() :: reset | ready | error | timeout.

-export_type([uart/0, uart_resource/0, uart_state/0]).

-define(NIF_TIMEOUT_MS, 10).
-define(READ_SIZE, 256).

%% @doc Open the devicetree-selected UART with the supplied options.
-spec open(Options :: [option()]) -> uart().
open(Options) ->
    case ?MODULE:init(Options) of
        {ok, Resource} ->
            spawn_link(fun() -> loop(Resource) end);
        {error, Reason} ->
            error(Reason)
    end.

%% @doc Open a named or indexed UART controller.
-spec open(Name :: peripheral(), Options :: [option()]) -> uart().
open(Name, Options) ->
    open([{peripheral, Name} | Options]).

%% @doc Close a UART handle.
-spec close(UART :: uart()) -> ok | {error, term()}.
close(Pid) when is_pid(Pid) ->
    call(Pid, close).

%% @doc Read currently available UART data without waiting.
-spec read(UART :: uart()) -> {ok, binary()} | {error, term()}.
read(Pid) when is_pid(Pid) ->
    call(Pid, {read, 0}).

%% @doc Wait up to `Timeout' milliseconds for UART data.
-spec read(UART :: uart(), Timeout :: timeout()) -> {ok, binary()} | {error, term()}.
read(Pid, Timeout) when is_pid(Pid), Timeout =:= infinity ->
    call(Pid, {read, infinity});
read(Pid, Timeout) when is_pid(Pid), is_integer(Timeout), Timeout >= 0 ->
    call(Pid, {read, Timeout}).

%% @doc Write all bytes in an iolist to a UART handle.
-spec write(UART :: uart(), Data :: iodata()) -> ok | {error, term()}.
write(Pid, Data) when is_pid(Pid) ->
    call(Pid, {write, erlang:iolist_to_binary(Data)}).

%% @doc Initialize a Zephyr UART resource.
-spec init(Options :: [option()]) -> {ok, uart_resource()} | {error, term()}.
init(_Options) ->
    erlang:nif_error(undefined).

%% @doc Release a Zephyr UART resource.
-spec deinit(Resource :: uart_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%% @doc Write for at most 10 ms in one scheduler-safe native operation.
-spec write(Resource :: uart_resource(), Data :: binary(), Timeout :: timeout()) ->
    non_neg_integer() | {error, term()}.
write(_Resource, _Data, _Timeout) ->
    erlang:nif_error(undefined).

%% @doc Read for at most 10 ms in one scheduler-safe native operation.
-spec read(Resource :: uart_resource(), Count :: 0..65535, Timeout :: timeout()) ->
    {ok, binary()} | {error, term()}.
read(_Resource, _Count, _Timeout) ->
    erlang:nif_error(undefined).

%% @doc Abort the current UART operation.
-spec abort(Resource :: uart_resource()) -> ok | {error, term()}.
abort(_Resource) ->
    erlang:nif_error(undefined).

%% @doc Return the last UART state.
-spec get_state(Resource :: uart_resource()) -> uart_state().
get_state(_Resource) ->
    erlang:nif_error(undefined).

%% @doc Return the last Zephyr UART error number, or zero.
-spec get_error(Resource :: uart_resource()) -> non_neg_integer().
get_error(_Resource) ->
    erlang:nif_error(undefined).

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
        {From, Ref, close} ->
            From ! {Ref, ?MODULE:deinit(Resource)};
        {From, Ref, {read, Timeout}} ->
            From ! {Ref, read_until_data(Resource, Timeout)},
            loop(Resource);
        {From, Ref, {write, Data}} ->
            From ! {Ref, write_all(Resource, Data)},
            loop(Resource)
    end.

read_until_data(Resource, 0) ->
    read_available(Resource);
read_until_data(Resource, infinity) ->
    case ?MODULE:read(Resource, 1, ?NIF_TIMEOUT_MS) of
        {ok, First} -> {ok, drain_available(Resource, [First])};
        {error, timeout} -> read_until_data(Resource, infinity);
        {error, _} = Error -> Error
    end;
read_until_data(Resource, Timeout) ->
    Started = erlang:monotonic_time(millisecond),
    read_until_deadline(Resource, Started + Timeout).

read_until_deadline(Resource, Deadline) ->
    Remaining = Deadline - erlang:monotonic_time(millisecond),
    if
        Remaining =< 0 ->
            read_available(Resource);
        true ->
            Slice = erlang:min(Remaining, ?NIF_TIMEOUT_MS),
            case ?MODULE:read(Resource, 1, Slice) of
                {ok, First} -> {ok, drain_available(Resource, [First])};
                {error, timeout} -> read_until_deadline(Resource, Deadline);
                {error, _} = Error -> Error
            end
    end.

read_available(Resource) ->
    case ?MODULE:read(Resource, ?READ_SIZE, 0) of
        {ok, Data} -> {ok, Data};
        {error, _} = Error -> Error
    end.

drain_available(Resource, Acc) ->
    case ?MODULE:read(Resource, ?READ_SIZE, 0) of
        {ok, Data} -> drain_available(Resource, [Data | Acc]);
        {error, timeout} -> erlang:iolist_to_binary(lists:reverse(Acc));
        {error, _} -> erlang:iolist_to_binary(lists:reverse(Acc))
    end.

write_all(_Resource, <<>>) ->
    ok;
write_all(Resource, Data) ->
    case ?MODULE:write(Resource, Data, ?NIF_TIMEOUT_MS) of
        Written when is_integer(Written), Written > 0 ->
            <<_:Written/binary, Rest/binary>> = Data,
            write_all(Resource, Rest);
        0 ->
            {error, timeout};
        {error, _} = Error ->
            Error
    end.
