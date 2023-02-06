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

-module(test_gen_event).

-export([test/0]).
-export([init/1, handle_event/2, terminate/2]).

-record(state, {
    pid
}).

test() ->
    ok = test_add_handler(),
    ok = test_delete_handler(),
    ok = test_sync_notify(),
    ok = test_crash_handler_init(),
    ok = test_crash_handler_notify(),
    ok.

test_add_handler() ->
    {ok, Pid} = gen_event:start(),

    ok = gen_event:add_handler(Pid, ?MODULE, self()),

    ok = gen_event:notify(Pid, ok),
    receive
        ok ->
            ok
    end,

    ok = gen_event:stop(Pid),
    ok.

test_delete_handler() ->
    {ok, Pid} = gen_event:start(),

    ok = gen_event:add_handler(Pid, ?MODULE, self()),

    ok = gen_event:notify(Pid, ok),
    receive
        ok ->
            ok
    end,

    {error, module_not_found} = gen_event:delete_handler(Pid, no_such_handler, whatever),

    let_me_know = gen_event:delete_handler(Pid, ?MODULE, let_me_know),
    {error, module_not_found} = gen_event:delete_handler(Pid, ?MODULE, whatever),

    ok = gen_event:add_handler(Pid, ?MODULE, self()),
    crashme = gen_event:delete_handler(Pid, ?MODULE, crashme),

    ok = gen_event:stop(Pid),
    ok.

test_sync_notify() ->
    {ok, Pid} = gen_event:start(),

    Self = self(),
    WaitingPid = spawn(fun() -> wait_for_ok(Self) end),
    ok = gen_event:add_handler(Pid, ?MODULE, WaitingPid),

    ok = gen_event:sync_notify(Pid, ok),
    receive
        notified ->
            ok
    end,

    ok = gen_event:stop(Pid),
    ok.

wait_for_ok(Pid) ->
    receive
        ok ->
            Pid ! notified
    end.

test_crash_handler_init() ->
    {ok, Pid} = gen_event:start(),

    {error, return_error} = gen_event:add_handler(Pid, ?MODULE, return_error),
    crashme = gen_event:add_handler(Pid, ?MODULE, crashme),

    ok = gen_event:stop(Pid),
    ok.

test_crash_handler_notify() ->
    {ok, Pid} = gen_event:start(),

    ok = gen_event:add_handler(Pid, ?MODULE, self()),

    ok = gen_event:notify(Pid, return_error),
    ok =
        receive
            _ ->
                fail
        after 100 ->
            ok
        end,

    ok = gen_event:notify(Pid, crashme),
    ok =
        receive
            _ ->
                fail
        after 100 ->
            ok
        end,

    ok = gen_event:stop(Pid),
    ok.

%%
%% callbacks
%%

init(return_error) ->
    {error, return_error};
init(crashme) ->
    throw(crashme);
init(Pid) when is_pid(Pid) ->
    {ok, #state{pid = Pid}}.

handle_event(return_error, _State) ->
    {error, return_error};
handle_event(crashme, _State) ->
    throw(crashme);
handle_event(Event, State) ->
    State#state.pid ! Event,
    {ok, State}.

terminate(let_me_know, _State) ->
    let_me_know;
terminate(crashme, _State) ->
    throw(crashme);
terminate(_Args, _State) ->
    ok.
