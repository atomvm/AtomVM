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
%% </ul>
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
    read_bytes/3, read_bytes/4,
    write_bytes/2, write_bytes/3, write_bytes/4
]).
-export([
    init/1,
    deinit/1,
    master_transmit/4,
    master_receive/4,
    mem_read/6,
    mem_write/6
]).

-define(DEFAULT_SEND_TIMEOUT_MS, 500).

-type i2c_resource() :: reference().
-type i2c() :: pid().
-type address() :: 0..127.

-export_type([i2c/0, i2c_resource/0, address/0]).

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
close(Pid) ->
    call(Pid, close).

-spec begin_transmission(I2C :: i2c(), Address :: address()) -> ok | {error, term()}.
begin_transmission(Pid, Address) ->
    call(Pid, {begin_transmission, Address}).

-spec write_byte(I2C :: i2c(), Byte :: byte()) -> ok | {error, term()}.
write_byte(Pid, Byte) ->
    call(Pid, {write_byte, Byte}).

-spec write_bytes(I2C :: i2c(), Bytes :: binary()) -> ok | {error, term()}.
write_bytes(Pid, Bytes) ->
    call(Pid, {write_bytes_tx, Bytes}).

-spec end_transmission(I2C :: i2c()) -> ok | {error, term()}.
end_transmission(Pid) ->
    call(Pid, end_transmission).

-spec read_bytes(I2C :: i2c(), Address :: address(), Count :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
read_bytes(Pid, Address, Count) ->
    call(Pid, {read_bytes, Address, Count}).

-spec read_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: non_neg_integer(),
    Count :: non_neg_integer()
) -> {ok, binary()} | {error, term()}.
read_bytes(Pid, Address, Register, Count) ->
    call(Pid, {read_bytes, Address, Register, Count}).

-spec write_bytes(I2C :: i2c(), Address :: address(), Data :: binary() | byte()) ->
    ok | {error, term()}.
write_bytes(Pid, Address, Data) when is_integer(Data) ->
    write_bytes(Pid, Address, <<Data:8>>);
write_bytes(Pid, Address, Data) ->
    call(Pid, {write_bytes, Address, Data}).

-spec write_bytes(
    I2C :: i2c(),
    Address :: address(),
    Register :: non_neg_integer(),
    Data :: binary() | byte()
) -> ok | {error, term()}.
write_bytes(Pid, Address, Register, Data) when is_integer(Data) ->
    write_bytes(Pid, Address, Register, <<Data:8>>);
write_bytes(Pid, Address, Register, Data) ->
    call(Pid, {write_bytes, Address, Register, Data}).

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

%% ---------------------------------------------------------------------------
%% Internal helpers
%% ---------------------------------------------------------------------------

get_value(_Key, [], Default) -> Default;
get_value(Key, [{Key, Value} | _], _Default) -> Value;
get_value(Key, [_ | Rest], Default) -> get_value(Key, Rest, Default).

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
handle_request(_Resource, _SendTimeoutMs, TxState, _Unknown) ->
    {reply, {error, badarg}, TxState}.

mem_add_size(MemAddr) when MemAddr > 16#FF -> 16;
mem_add_size(_MemAddr) -> 8.
