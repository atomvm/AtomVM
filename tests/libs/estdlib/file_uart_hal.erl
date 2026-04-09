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

%%-----------------------------------------------------------------------------
%% @doc UART HAL for BEAM using an Erlang port and socat.
%%
%% This module provides a UART interface for BEAM by spawning a socat
%% process that bridges between stdin/stdout and a PTY device. Data
%% written to the port goes to the PTY; data arriving on the PTY is
%% delivered as port messages.
%%
%% Intended only for testing serial distribution over socat virtual
%% serial ports. On AtomVM, use the platform-specific `uart' module.
%% @end
%%-----------------------------------------------------------------------------
-module(file_uart_hal).

-behaviour(uart_hal).
-behaviour(gen_server).

-export([open/1, open/2, close/1, read/1, read/2, write/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    port :: port(),
    buffer :: binary(),
    waiter :: {term(), reference() | undefined} | undefined
}).

-spec open(list()) -> pid() | {error, term()}.
open(Opts) ->
    {ok, Pid} = gen_server:start(?MODULE, Opts, []),
    Pid.

-spec open(uart_hal:peripheral(), list()) -> pid() | {error, term()}.
open(_Name, Opts) ->
    open(Opts).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:stop(Pid).

-spec read(pid()) -> {ok, binary()} | {error, term()}.
read(Pid) ->
    gen_server:call(Pid, {read, infinity}, infinity).

-spec read(pid(), pos_integer() | infinity) -> {ok, binary()} | {error, term()}.
read(Pid, infinity) ->
    read(Pid);
read(Pid, Timeout) ->
    gen_server:call(Pid, {read, Timeout}, Timeout + 5000).

-spec write(pid(), iodata()) -> ok.
write(Pid, Data) ->
    gen_server:call(Pid, {write, Data}, infinity).

%% gen_server callbacks

init(Opts) ->
    Peripheral = proplists:get_value(peripheral, Opts),
    Cmd = "socat -b 65536 - " ++ Peripheral ++ ",rawer",
    Port = open_port({spawn, Cmd}, [binary, stream, exit_status]),
    {ok, #state{port = Port, buffer = <<>>, waiter = undefined}}.

handle_call({read, _}, _From, #state{buffer = Buffer} = State) when
    byte_size(Buffer) > 0
->
    {reply, {ok, Buffer}, State#state{buffer = <<>>}};
handle_call({read, infinity}, From, #state{buffer = <<>>} = State) ->
    {noreply, State#state{waiter = {From, undefined}}};
handle_call({read, Timeout}, From, #state{buffer = <<>>} = State) ->
    Ref = erlang:send_after(Timeout, self(), read_timeout),
    {noreply, State#state{waiter = {From, Ref}}};
handle_call({write, Data}, _From, #state{port = Port} = State) ->
    port_command(Port, iolist_to_binary(Data)),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port = Port, waiter = undefined, buffer = Buffer} = State) ->
    {noreply, State#state{buffer = <<Buffer/binary, Data/binary>>}};
handle_info(
    {Port, {data, Data}}, #state{port = Port, waiter = {From, TimerRef}, buffer = Buffer} = State
) ->
    cancel_timer(TimerRef),
    gen_server:reply(From, {ok, <<Buffer/binary, Data/binary>>}),
    {noreply, State#state{buffer = <<>>, waiter = undefined}};
handle_info(read_timeout, #state{waiter = {From, _}} = State) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, State#state{waiter = undefined}};
handle_info(read_timeout, State) ->
    {noreply, State};
handle_info({Port, {exit_status, _}}, #state{port = Port, waiter = Waiter} = State) ->
    case Waiter of
        {From, TimerRef} ->
            cancel_timer(TimerRef),
            gen_server:reply(From, {error, port_closed});
        undefined ->
            ok
    end,
    {stop, port_closed, State#state{waiter = undefined}}.

terminate(_Reason, #state{port = Port}) ->
    catch port_close(Port),
    ok.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).
