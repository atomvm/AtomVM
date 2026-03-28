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
%% @doc Mock UART HAL for testing serial distribution without hardware.
%%
%% Creates pairs of connected virtual UART endpoints. Data written to
%% one endpoint becomes readable on the other, simulating a serial link.
%%
%% ```
%% {A, B} = mock_uart_hal:create_pair(),
%% ok = mock_uart_hal:write(A, <<"hello">>),
%% {ok, <<"hello">>} = mock_uart_hal:read(B).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(mock_uart_hal).

-behaviour(uart_hal).
-behaviour(gen_server).

-export([create_pair/0, open/1, open/2, close/1, read/1, read/2, write/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    peer :: pid() | undefined,
    buffer :: binary(),
    waiter :: {term(), reference() | undefined} | undefined
}).

%% @doc Create a pair of connected virtual UART endpoints.
%% Data written to the first pid is readable from the second, and vice versa.
-spec create_pair() -> {pid(), pid()}.
create_pair() ->
    {ok, A} = gen_server:start(?MODULE, [], []),
    {ok, B} = gen_server:start(?MODULE, [], []),
    ok = gen_server:call(A, {set_peer, B}),
    ok = gen_server:call(B, {set_peer, A}),
    {A, B}.

%% @doc Open a mock UART endpoint.
%% Accepts `{mock_pid, Pid}' in Opts where Pid is an endpoint
%% previously returned by {@link create_pair/0}.
-spec open(proplists:proplist()) -> pid() | {error, term()}.
open(Opts) ->
    case proplists:get_value(mock_pid, Opts) of
        undefined -> {error, no_mock_pid};
        Pid when is_pid(Pid) -> Pid
    end.

%% @doc Open with peripheral name (ignored for mock).
-spec open(term(), proplists:proplist()) -> pid() | {error, term()}.
open(_Name, Opts) ->
    open(Opts).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:stop(Pid).

%% @doc Blocking read — waits until data is available.
%% This matches the behavior expected by serial_dist_controller's
%% reader_loop, which calls read/1 in a tight loop.
-spec read(pid()) -> {ok, binary()} | {error, term()}.
read(Pid) ->
    gen_server:call(Pid, {read, infinity}, infinity).

%% @doc Read with timeout in milliseconds.
%% Returns `{error, timeout}' if no data arrives within the timeout.
-spec read(pid(), pos_integer() | infinity) -> {ok, binary()} | {error, term()}.
read(Pid, infinity) ->
    read(Pid);
read(Pid, Timeout) ->
    gen_server:call(Pid, {read, Timeout}, Timeout + 5000).

%% @doc Write data to the UART. Data becomes readable on the peer endpoint.
-spec write(pid(), iodata()) -> ok.
write(Pid, Data) ->
    gen_server:call(Pid, {write, iolist_to_binary(Data)}).

%% gen_server callbacks

init([]) ->
    {ok, #state{peer = undefined, buffer = <<>>, waiter = undefined}}.

handle_call({set_peer, Peer}, _From, State) ->
    {reply, ok, State#state{peer = Peer}};
handle_call({read, _Timeout}, _From, #state{buffer = Buffer} = State) when
    byte_size(Buffer) > 0
->
    {reply, {ok, Buffer}, State#state{buffer = <<>>}};
handle_call({read, infinity}, From, #state{buffer = <<>>} = State) ->
    {noreply, State#state{waiter = {From, undefined}}};
handle_call({read, Timeout}, From, #state{buffer = <<>>} = State) ->
    Ref = erlang:send_after(Timeout, self(), read_timeout),
    {noreply, State#state{waiter = {From, Ref}}};
handle_call({write, Data}, _From, #state{peer = Peer} = State) ->
    Peer ! {mock_uart_data, Data},
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mock_uart_data, Data}, #state{waiter = undefined, buffer = Buffer} = State) ->
    {noreply, State#state{buffer = <<Buffer/binary, Data/binary>>}};
handle_info({mock_uart_data, Data}, #state{waiter = {From, TimerRef}, buffer = Buffer} = State) ->
    cancel_timer(TimerRef),
    gen_server:reply(From, {ok, <<Buffer/binary, Data/binary>>}),
    {noreply, State#state{buffer = <<>>, waiter = undefined}};
handle_info(read_timeout, #state{waiter = {From, _}} = State) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, State#state{waiter = undefined}};
handle_info(read_timeout, State) ->
    %% Stale timer after data arrived — ignore
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).
