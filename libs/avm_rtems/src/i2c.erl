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
%% @doc I2C implementation for RTEMS using Linux-compatible I2C ioctls.
%%
%% This module implements the {@link i2c_hal} behaviour. Transfers use
%% `I2C_RDWR' on a registered `/dev/i2c-*' node. The i.MX7 BSP registers the
%% controller on first `open/1'; SPARC erc32 has no I2C controller and
%% `init/1' returns `{error, enotsup}'.
%%
%% Options:
%% <ul>
%% <li>`{peripheral, Path}' - device path (default `"/dev/i2c-0"')</li>
%% <li>`{fdt_alias, Alias}' - FDT alias such as `"i2c0"' (imx7 only)</li>
%% <li>`{retries, N}' - bus retry count for slave transfers (imx7 only)</li>
%% <li>`{send_timeout_ms, Ms}' - default timeout in milliseconds (default: 500)</li>
%% </ul>
%%
%% Addresses can be 7-bit (`0..127') or 10-bit (`128..1023'). Addresses in the
%% range `128..1023' automatically set the `I2C_M_TEN' flag.
%% @end
%%-----------------------------------------------------------------------------
-module(i2c).

-behaviour(i2c_hal).

-export([
    open/1,
    close/1,
    begin_transmission/2,
    write_byte/2,
    end_transmission/1,
    read_bytes/3, read_bytes/4, read_bytes/5,
    write_bytes/2, write_bytes/3, write_bytes/4, write_bytes/5,
    is_device_ready/2, is_device_ready/3, is_device_ready/4,
    scan/1, scan/3,
    transfer/2, transfer/3
]).
-export([
    init/1,
    deinit/1,
    master_transmit/4,
    master_receive/4,
    mem_read/6,
    mem_write/6,
    is_device_ready_nif/4,
    transfer_nif/3
]).

-define(DEFAULT_SEND_TIMEOUT_MS, 500).
-define(DEFAULT_PROBE_TIMEOUT_MS, 100).

-type i2c_resource() :: reference().
-type i2c() :: pid().
-type address() :: 0..1023.
-type mem_add_size() :: 8 | 16.
-type transfer_msg() ::
    {write, address(), binary() | byte()}
    | {read, address(), non_neg_integer()}.

-export_type([i2c/0, i2c_resource/0, address/0, mem_add_size/0, transfer_msg/0]).

%% ---------------------------------------------------------------------------
%% High-level API
%% ---------------------------------------------------------------------------

-spec open(Params :: [{atom(), term()}]) -> i2c().
open(Params) ->
    SendTimeoutMs = get_value(send_timeout_ms, Params, ?DEFAULT_SEND_TIMEOUT_MS),
    case ?MODULE:init(Params) of
        {ok, Resource} ->
            erlang:spawn_opt(fun() -> loop(Resource, SendTimeoutMs, undefined) end, [link]);
        {error, Reason} ->
            error(Reason)
    end.

-spec close(I2C :: i2c()) -> ok | {error, term()}.
close(Pid) when is_pid(Pid) ->
    call(Pid, close).

-spec begin_transmission(I2C :: i2c(), Address :: address()) -> ok | {error, term()}.
begin_transmission(Pid, Address) when is_pid(Pid) ->
    call(Pid, {begin_transmission, Address}).

-spec write_byte(I2C :: i2c(), Byte :: byte()) -> ok | {error, term()}.
write_byte(Pid, Byte) when is_pid(Pid) ->
    call(Pid, {write_byte, Byte}).

-spec write_bytes(I2C :: i2c(), Bytes :: binary()) -> ok | {error, term()}.
write_bytes(Pid, Bytes) when is_pid(Pid) ->
    call(Pid, {write_bytes_tx, Bytes}).

-spec end_transmission(I2C :: i2c()) -> ok | {error, term()}.
end_transmission(Pid) when is_pid(Pid) ->
    call(Pid, end_transmission).

-spec read_bytes(I2C :: i2c(), Address :: address(), Count :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
read_bytes(Pid, Address, Count) when is_pid(Pid) ->
    call(Pid, {read_bytes, Address, Count}).

-spec read_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: non_neg_integer(),
    Count :: non_neg_integer()
) -> {ok, binary()} | {error, term()}.
read_bytes(Pid, Address, Register, Count) when is_pid(Pid) ->
    call(Pid, {read_bytes, Address, Register, Count}).

-spec read_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: non_neg_integer(),
    Count :: non_neg_integer(),
    Opts :: [{atom(), term()}] | map()
) -> {ok, binary()} | {error, term()}.
read_bytes(Pid, Address, Register, Count, Opts) when is_pid(Pid) ->
    MemAddSize = get_mem_add_size(Opts, Register),
    call(Pid, {read_bytes, Address, Register, Count, MemAddSize}).

-spec write_bytes(I2C :: i2c(), Address :: address(), Data :: binary() | byte()) ->
    ok | {error, term()}.
write_bytes(Pid, Address, Data) when is_pid(Pid), is_integer(Data) ->
    write_bytes(Pid, Address, <<Data:8>>);
write_bytes(Pid, Address, Data) when is_pid(Pid) ->
    call(Pid, {write_bytes, Address, Data}).

-spec write_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: non_neg_integer(),
    Data :: binary() | byte()
) -> ok | {error, term()}.
write_bytes(Pid, Address, Register, Data) when is_pid(Pid), is_integer(Data) ->
    write_bytes(Pid, Address, Register, <<Data:8>>);
write_bytes(Pid, Address, Register, Data) when is_pid(Pid) ->
    call(Pid, {write_bytes, Address, Register, Data}).

-spec write_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: non_neg_integer(),
    Data :: binary() | byte(),
    Opts :: [{atom(), term()}] | map()
) -> ok | {error, term()}.
write_bytes(Pid, Address, Register, Data, Opts) when is_pid(Pid), is_integer(Data) ->
    write_bytes(Pid, Address, Register, <<Data:8>>, Opts);
write_bytes(Pid, Address, Register, Data, Opts) when is_pid(Pid) ->
    MemAddSize = get_mem_add_size(Opts, Register),
    call(Pid, {write_bytes, Address, Register, Data, MemAddSize}).

-spec is_device_ready(I2C :: i2c() | i2c_resource(), Address :: address()) ->
    ok | {error, nack | timeout | term()}.
is_device_ready(I2C, Address) ->
    is_device_ready(I2C, Address, 1, ?DEFAULT_SEND_TIMEOUT_MS).

-spec is_device_ready(
    I2C :: i2c() | i2c_resource(),
    Address :: address(),
    TimeoutMs :: timeout()
) -> ok | {error, nack | timeout | term()}.
is_device_ready(I2C, Address, TimeoutMs) when is_integer(TimeoutMs); TimeoutMs =:= infinity ->
    is_device_ready(I2C, Address, 1, TimeoutMs).

-spec is_device_ready(
    I2C :: i2c() | i2c_resource(),
    Address :: address(),
    Trials :: pos_integer(),
    TimeoutMs :: timeout()
) -> ok | {error, nack | timeout | term()}.
is_device_ready(Pid, Address, Trials, TimeoutMs) when is_pid(Pid) ->
    call(Pid, {is_device_ready, Address, Trials, TimeoutMs});
is_device_ready(Resource, Address, Trials, TimeoutMs) when is_reference(Resource) ->
    case ?MODULE:is_device_ready_nif(Resource, Address, Trials, TimeoutMs) of
        {error, enxio} -> {error, nack};
        {error, eio} -> {error, nack};
        Result -> Result
    end.

-spec scan(I2C :: i2c() | i2c_resource()) -> {ok, [address()]} | {error, term()}.
scan(I2C) ->
    scan(I2C, 16#08, 16#77).

-spec scan(
    I2C :: i2c() | i2c_resource(),
    StartAddr :: address(),
    EndAddr :: address()
) -> {ok, [address()]} | {error, term()}.
scan(I2C, StartAddr, EndAddr) when
    is_integer(StartAddr), is_integer(EndAddr),
    StartAddr >= 0, EndAddr =< 1023, StartAddr =< EndAddr
->
    do_scan(I2C, StartAddr, EndAddr, []);
scan(_I2C, _StartAddr, _EndAddr) ->
    {error, badarg}.

do_scan(_I2C, Current, End, Acc) when Current > End ->
    {ok, lists:reverse(Acc)};
do_scan(I2C, Current, End, Acc) ->
    case is_device_ready(I2C, Current, 1, ?DEFAULT_PROBE_TIMEOUT_MS) of
        ok ->
            do_scan(I2C, Current + 1, End, [Current | Acc]);
        {error, nack} ->
            do_scan(I2C, Current + 1, End, Acc);
        {error, Reason} when Reason =:= enotsup; Reason =:= enodev; Reason =:= einval ->
            {error, Reason};
        {error, {server_died, _}} = Err ->
            Err;
        {error, _Other} ->
            do_scan(I2C, Current + 1, End, Acc)
    end.

-spec transfer(I2C :: i2c() | i2c_resource(), Messages :: [transfer_msg()]) ->
    {ok, [ok | binary()]} | {error, term()}.
transfer(I2C, Messages) ->
    transfer(I2C, Messages, ?DEFAULT_SEND_TIMEOUT_MS).

-spec transfer(
    I2C :: i2c() | i2c_resource(),
    Messages :: [transfer_msg()],
    TimeoutMs :: timeout()
) ->
    {ok, [ok | binary()]} | {error, term()}.
transfer(Pid, Messages, TimeoutMs) when is_pid(Pid) ->
    call(Pid, {transfer, normalize_transfer_msgs(Messages), TimeoutMs});
transfer(Resource, Messages, TimeoutMs) when is_reference(Resource) ->
    ?MODULE:transfer_nif(Resource, normalize_transfer_msgs(Messages), TimeoutMs).

%% ---------------------------------------------------------------------------
%% Low-level NIFs
%% ---------------------------------------------------------------------------

-spec init(Opts :: [{atom(), term()}]) -> {ok, i2c_resource()} | {error, term()}.
init(_Opts) ->
    erlang:nif_error(undefined).

-spec deinit(Resource :: i2c_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

-spec master_transmit(
    Resource :: i2c_resource(),
    Address :: address(),
    Data :: binary(),
    TimeoutMs :: timeout()
) -> non_neg_integer() | {error, term()}.
master_transmit(_Resource, _Address, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

-spec master_receive(
    Resource :: i2c_resource(),
    Address :: address(),
    Count :: non_neg_integer(),
    TimeoutMs :: timeout()
) -> {ok, binary()} | {error, term()}.
master_receive(_Resource, _Address, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

-spec mem_read(
    Resource :: i2c_resource(),
    Address :: address(),
    Register :: non_neg_integer(),
    MemAddSize :: 8 | 16,
    Count :: non_neg_integer(),
    TimeoutMs :: timeout()
) -> {ok, binary()} | {error, term()}.
mem_read(_Resource, _Address, _Register, _MemAddSize, _Count, _TimeoutMs) ->
    erlang:nif_error(undefined).

-spec mem_write(
    Resource :: i2c_resource(),
    Address :: address(),
    Register :: non_neg_integer(),
    MemAddSize :: 8 | 16,
    Data :: binary(),
    TimeoutMs :: timeout()
) -> non_neg_integer() | {error, term()}.
mem_write(_Resource, _Address, _Register, _MemAddSize, _Data, _TimeoutMs) ->
    erlang:nif_error(undefined).

-spec is_device_ready_nif(
    Resource :: i2c_resource(),
    Address :: address(),
    Trials :: pos_integer(),
    TimeoutMs :: timeout()
) -> ok | {error, term()}.
is_device_ready_nif(_Resource, _Address, _Trials, _TimeoutMs) ->
    erlang:nif_error(undefined).

-spec transfer_nif(
    Resource :: i2c_resource(),
    Messages :: [transfer_msg()],
    TimeoutMs :: timeout()
) -> {ok, [ok | binary()]} | {error, term()}.
transfer_nif(_Resource, _Messages, _TimeoutMs) ->
    erlang:nif_error(undefined).

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

get_value(_Key, [], Default) -> Default;
get_value(Key, [{Key, Value} | _], _Default) -> Value;
get_value(Key, [_ | Rest], Default) -> get_value(Key, Rest, Default).

get_mem_add_size(Opts, Register) when is_list(Opts) ->
    case get_value(mem_add_size, Opts, undefined) of
        8 -> 8;
        16 -> 16;
        undefined -> mem_add_size(Register);
        _Other -> error(badarg)
    end;
get_mem_add_size(Opts, Register) when is_map(Opts) ->
    case maps:get(mem_add_size, Opts, undefined) of
        8 -> 8;
        16 -> 16;
        undefined -> mem_add_size(Register);
        _Other -> error(badarg)
    end;
get_mem_add_size(_Opts, _Register) ->
    error(badarg).

normalize_transfer_msgs([]) -> [];
normalize_transfer_msgs([{write, Addr, Byte} | Rest]) when is_integer(Byte) ->
    [{write, Addr, <<Byte:8>>} | normalize_transfer_msgs(Rest)];
normalize_transfer_msgs([{write, _Addr, Bin} = Msg | Rest]) when is_binary(Bin) ->
    [Msg | normalize_transfer_msgs(Rest)];
normalize_transfer_msgs([{read, _Addr, Count} = Msg | Rest]) when is_integer(Count), Count >= 0 ->
    [Msg | normalize_transfer_msgs(Rest)];
normalize_transfer_msgs(_) ->
    error(badarg).

call(Pid, Request) ->
    MRef = monitor(process, Pid),
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Reply} ->
            demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, process, Pid, Reason} ->
            {error, {server_died, Reason}}
    end.

loop(Resource, SendTimeoutMs, TxState) ->
    receive
        {From, Ref, Request} ->
            case handle_request(Resource, SendTimeoutMs, TxState, Request) of
                {reply, Reply, stop} ->
                    From ! {Ref, Reply};
                {reply, Reply, NewTxState} ->
                    From ! {Ref, Reply},
                    loop(Resource, SendTimeoutMs, NewTxState)
            end
    end.

handle_request(Resource, _SendTimeoutMs, _TxState, close) ->
    ?MODULE:deinit(Resource),
    {reply, ok, stop};
handle_request(_Resource, _SendTimeoutMs, undefined, {begin_transmission, Address}) ->
    {reply, ok, {Address, []}};
handle_request(_Resource, _SendTimeoutMs, {Address, Acc}, {begin_transmission, _NewAddress}) ->
    {reply, {error, transaction_already_in_progress}, {Address, Acc}};
handle_request(_Resource, _SendTimeoutMs, {Address, Acc}, {write_byte, Byte}) ->
    {reply, ok, {Address, [<<Byte:8>> | Acc]}};
handle_request(_Resource, _SendTimeoutMs, undefined, {write_byte, _Byte}) ->
    {reply, {error, no_transaction}, undefined};
handle_request(_Resource, _SendTimeoutMs, {Address, Acc}, {write_bytes_tx, Bytes}) ->
    {reply, ok, {Address, [Bytes | Acc]}};
handle_request(_Resource, _SendTimeoutMs, undefined, {write_bytes_tx, _Bytes}) ->
    {reply, {error, no_transaction}, undefined};
handle_request(Resource, SendTimeoutMs, {Address, Acc}, end_transmission) ->
    Data = erlang:iolist_to_binary(lists:reverse(Acc)),
    Result =
        case ?MODULE:master_transmit(Resource, Address, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, undefined};
handle_request(_Resource, _SendTimeoutMs, undefined, end_transmission) ->
    {reply, {error, no_transaction}, undefined};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Count}) ->
    Result = ?MODULE:master_receive(Resource, Address, Count, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Register, Count}) ->
    MemAddSize = mem_add_size(Register),
    Result = ?MODULE:mem_read(Resource, Address, Register, MemAddSize, Count, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {read_bytes, Address, Register, Count, MemAddSize}) ->
    Result = ?MODULE:mem_read(Resource, Address, Register, MemAddSize, Count, SendTimeoutMs),
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Data}) ->
    Result =
        case ?MODULE:master_transmit(Resource, Address, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Register, Data}) ->
    MemAddSize = mem_add_size(Register),
    Result =
        case ?MODULE:mem_write(Resource, Address, Register, MemAddSize, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState};
handle_request(Resource, SendTimeoutMs, TxState, {write_bytes, Address, Register, Data, MemAddSize}) ->
    Result =
        case ?MODULE:mem_write(Resource, Address, Register, MemAddSize, Data, SendTimeoutMs) of
            {error, _} = Error -> Error;
            _N -> ok
        end,
    {reply, Result, TxState};
handle_request(Resource, _SendTimeoutMs, undefined, {is_device_ready, Address, Trials, TimeoutMs}) ->
    Result =
        case ?MODULE:is_device_ready_nif(Resource, Address, Trials, TimeoutMs) of
            {error, enxio} -> {error, nack};
            {error, eio} -> {error, nack};
            Other -> Other
        end,
    {reply, Result, undefined};
handle_request(_Resource, _SendTimeoutMs, {_, _} = TxState, {is_device_ready, _Address, _Trials, _TimeoutMs}) ->
    {reply, {error, transaction_already_in_progress}, TxState};
handle_request(Resource, _SendTimeoutMs, undefined, {transfer, Messages, TimeoutMs}) ->
    Result = ?MODULE:transfer_nif(Resource, Messages, TimeoutMs),
    {reply, Result, undefined};
handle_request(_Resource, _SendTimeoutMs, {_, _} = TxState, {transfer, _Messages, _TimeoutMs}) ->
    {reply, {error, transaction_already_in_progress}, TxState};
handle_request(_Resource, _SendTimeoutMs, TxState, _Unknown) ->
    {reply, {error, badarg}, TxState}.

mem_add_size(MemAddr) when MemAddr > 16#FF -> 16;
mem_add_size(_MemAddr) -> 8.
