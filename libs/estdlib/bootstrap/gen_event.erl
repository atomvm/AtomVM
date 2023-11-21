%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(gen_event).

%%
%% This module provides a naive implementation of a strict subset of the
%% OTP gen_event interface.  Functionality is limited to adding and deleting
%% handlers, as well as async and synchronous notifications.  More
%% sophisticated uses of this interface are reserved for the future.
%%

-export([
    start/0, start/2,
    start_link/0, start_link/2,
    stop/1,
    add_handler/3,
    delete_handler/3,
    notify/2,
    sync_notify/2
]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

%%
%% api
%%

start() ->
    gen_server:start(?MODULE, undefined, []).

start(EventMgrName, Options) ->
    gen_server:start(EventMgrName, ?MODULE, undefined, Options).

start_link() ->
    gen_server:start_link(?MODULE, undefined, []).

start_link(EventMgrName, Options) ->
    gen_server:start_link(EventMgrName, ?MODULE, undefined, Options).

stop(EventManagerRef) ->
    gen_server:stop(EventManagerRef).

add_handler(EventMgrRef, Handler, Args) ->
    gen_server:call(EventMgrRef, {add_handler, Handler, Args}).

delete_handler(EventMgrRef, Handler, Args) ->
    gen_server:call(EventMgrRef, {delete_handler, Handler, Args}).

notify(EventMgrRef, Event) ->
    gen_server:cast(EventMgrRef, {notify, Event}).

sync_notify(EventMgrRef, Event) ->
    gen_server:call(EventMgrRef, {sync_notify, Event}).

%%
%% gen_server implementation
%%

-record(state, {
    handlers = []
}).

%% @hidden
init(undefined) ->
    {ok, #state{}}.

%% @hidden
handle_cast({notify, Event}, State) ->
    NewHandlers = try_notify_handlers(State#state.handlers, Event, []),
    {noreply, State#state{handlers = NewHandlers}};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call({add_handler, Handler, Args}, _From, State) ->
    {Reply, NewHandlers} =
        case try_init_handler(Handler, Args) of
            {ok, HandlerState} ->
                {ok, [{Handler, HandlerState} | State#state.handlers]};
            Error ->
                {Error, State#state.handlers}
        end,
    {reply, Reply, State#state{handlers = NewHandlers}};
handle_call({delete_handler, Handler, Args}, _From, State) ->
    {Reply, NewHandlers} =
        case maybe_terminate_handler(State#state.handlers, Handler, Args) of
            {ok, Term} ->
                {Term, lists:keydelete(Handler, 1, State#state.handlers)};
            Error ->
                {Error, State#state.handlers}
        end,
    {reply, Reply, State#state{handlers = NewHandlers}};
handle_call({sync_notify, Event}, _From, State) ->
    NewHandlers = try_notify_handlers(State#state.handlers, Event, []),
    {reply, ok, State#state{handlers = NewHandlers}};
handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

%% @hidden
handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, State) ->
    [
        try_terminate_handler(Handler, stop, HandlerState)
     || {Handler, HandlerState} <- State#state.handlers
    ].

%%
%% internal implementation
%%

try_init_handler(Handler, Args) ->
    try
        Handler:init(Args)
    catch
        _:E ->
            E
    end.

try_notify_handlers([], _Event, Accum) ->
    lists:reverse(Accum);
try_notify_handlers([{Handler, HandlerState} = H | Rest], Event, Accum) ->
    try
        case Handler:handle_event(Event, HandlerState) of
            {ok, NewHandlerState} ->
                NewAccum = [{Handler, NewHandlerState} | Accum],
                try_notify_handlers(Rest, Event, NewAccum);
            _Error ->
                try_notify_handlers(Rest, Event, [H | Accum])
        end
    catch
        _C:_E ->
            try_notify_handlers(Rest, Event, [H | Accum])
    end.

maybe_terminate_handler(Handlers, Handler, Args) ->
    case lists:keyfind(Handler, 1, Handlers) of
        false ->
            {error, module_not_found};
        {Handler, HandlerState} ->
            try_terminate_handler(Handler, Args, HandlerState)
    end.

try_terminate_handler(Handler, Args, HandlerState) ->
    try
        Term = Handler:terminate(Args, HandlerState),
        {ok, Term}
    catch
        _:E ->
            E
    end.
