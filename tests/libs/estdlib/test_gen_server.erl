%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
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

-module(test_gen_server).

-export([test/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    num_casts = 0,
    num_infos = 0,
    num_timeouts = 0,
    info_timeout = none
}).

test() ->
    ok = test_call(),
    ok = test_cast(),
    ok = test_info(),
    ok = test_start_link(),
    ok = test_init_exception(),
    ok = test_late_reply(),
    ok = test_concurrent_clients(),
    ok = test_timeout_call(),
    ok = test_timeout_cast(),
    ok = test_timeout_info(),
    ok = test_register(),
    ok.

test_call() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    pong = gen_server:call(Pid, ping),
    pong = gen_server:call(Pid, reply_ping),
    pong = gen_server:call(Pid, async_ping),

    gen_server:stop(Pid),
    ok.

test_start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),

    pong = gen_server:call(Pid, ping),
    pong = gen_server:call(Pid, reply_ping),
    erlang:process_flag(trap_exit, true),
    ok = gen_server:cast(Pid, crash),
    receive
        {'EXIT', Pid, _Reason} -> ok
    after 30000 -> timeout
    end.

test_cast() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),

    5 = gen_server:call(Pid, get_num_casts),
    0 = gen_server:call(Pid, get_num_casts),

    gen_server:stop(Pid),
    ok.

test_info() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    Pid ! ping,
    Pid ! ping,
    Pid ! ping,

    3 = gen_server:call(Pid, get_num_infos),
    0 = gen_server:call(Pid, get_num_infos),

    gen_server:stop(Pid),
    ok.

test_init_exception() ->
    try
        case gen_server:start(?MODULE, throwme, []) of
            {ok, _Pid} ->
                expected_error_on_exception;
            {error, _Reason} ->
                ok
        end
    catch
        _:E ->
            {did_not_expect_an_exception, E}
    end.

test_late_reply() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    %%
    %% just check to make sure messages make it back before the timeout
    %%
    ok = gen_server:call(Pid, {reply_after, 0, ok}),
    ok = gen_server:call(Pid, {reply_after, 150, ok}),
    %%
    %% this one should time out
    %%
    timeout =
        try gen_server:call(Pid, {reply_after, 300, ok}, 200) of
            unexpected -> unexpected
        catch
            % estdlib
            exit:timeout -> timeout;
            % OTP
            exit:{timeout, _} -> timeout;
            T:V -> {unexpected, T, V}
        end,
    %%
    %% flush the late message in the mailbox
    %% OTP's gen_server(3) man page says it's a two element tuple with a ref as
    %% a first element.
    %%
    receive
        {Ref, _} when is_reference(Ref) -> ok
    end,
    ok = gen_server:call(Pid, {reply_after, 0, ok}),
    gen_server:stop(Pid),
    ok.

test_concurrent_clients() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Self = self(),
    P1 = spawn(fun() -> make_requests(Pid, Self, 1, 1000) end),
    P2 = spawn(fun() -> make_requests(Pid, Self, 10, 100) end),
    P3 = spawn(fun() -> make_requests(Pid, Self, 20, 50) end),
    wait_for(P1),
    wait_for(P2),
    wait_for(P3),
    %%
    gen_server:stop(Pid),
    ok.

make_requests(_Pid, Waiting, _ReplyAfter, 0) ->
    Waiting ! self();
make_requests(Pid, Waiting, ReplyAfter, I) ->
    Ref = erlang:make_ref(),
    Ref = gen_server:call(Pid, {reply_after, ReplyAfter, Ref}),
    make_requests(Pid, Waiting, ReplyAfter, I - 1).

wait_for(P) ->
    receive
        P -> ok
    end.

test_timeout_call() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    0 = gen_server:call(Pid, get_num_timeouts),
    ok = gen_server:call(Pid, {call_reply_timeout, 10}),
    timer:sleep(11),
    1 = gen_server:call(Pid, get_num_timeouts),
    ok = gen_server:call(Pid, {call_reply_timeout, infinity}),
    timer:sleep(11),
    1 = gen_server:call(Pid, get_num_timeouts),
    try
        gen_server:call(Pid, {call_noreply_timeout, 10}, 100)
    catch
        _:_ -> ok
    end,
    2 = gen_server:call(Pid, get_num_timeouts),
    gen_server:stop(Pid),
    ok.

test_timeout_cast() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    0 = gen_server:call(Pid, get_num_timeouts),
    ok = gen_server:cast(Pid, {cast_timeout, 10}),
    timer:sleep(11),
    1 = gen_server:call(Pid, get_num_timeouts),
    ok = gen_server:cast(Pid, {cast_timeout, infinity}),
    timer:sleep(11),
    1 = gen_server:call(Pid, get_num_timeouts),
    gen_server:stop(Pid),
    ok.

test_timeout_info() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    0 = gen_server:call(Pid, get_num_timeouts),
    ok = gen_server:cast(Pid, {set_info_timeout, 10}),
    timer:sleep(11),
    0 = gen_server:call(Pid, get_num_timeouts),
    Pid ! ping,
    timer:sleep(11),
    N = gen_server:call(Pid, get_num_timeouts),
    true = N > 0,
    ok = test_timeout_info_repeats(Pid, 30),
    gen_server:stop(Pid).

% timeout message itself is repeated as it is a handle_info message.
% This test is flappy as there is no guarantee that Pid would be
% scheduled if the system is very busy.
test_timeout_info_repeats(_Pid, Sleep) when Sleep > 5000 -> fail;
test_timeout_info_repeats(Pid, Sleep) ->
    N = gen_server:call(Pid, get_num_timeouts),
    Pid ! ping,
    timer:sleep(Sleep),
    M = gen_server:call(Pid, get_num_timeouts),
    if
        M > N + 1 ->
            ok;
        true ->
            test_timeout_info_repeats(Pid, 2 * Sleep)
    end.

test_register() ->
    {ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    Pid = whereis(?MODULE),
    {error, {already_started, Pid}} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    gen_server:stop(Pid),
    % unregister doesn't work yet
    %   {ok, Pid2} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    %   gen_server:stop(Pid2),
    ok.

%%
%% callbacks
%%

init(throwme) ->
    throw(throwme);
init(_) ->
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(reply_ping, From, State) ->
    gen_server:reply(From, pong),
    {noreply, State};
handle_call(async_ping, From, State) ->
    erlang:spawn(gen_server, reply, [From, pong]),
    {noreply, State};
handle_call({reply_after, Ms, Reply}, From, State) ->
    spawn(fun() ->
        timer:sleep(Ms),
        gen_server:reply(From, Reply)
    end),
    {noreply, State};
handle_call(get_num_casts, From, #state{num_casts = NumCasts} = State) ->
    gen_server:reply(From, NumCasts),
    {noreply, State#state{num_casts = 0}};
handle_call(get_num_infos, From, #state{num_infos = NumInfos} = State) ->
    gen_server:reply(From, NumInfos),
    {noreply, State#state{num_infos = 0}};
handle_call(get_num_timeouts, _From, State) ->
    {reply, State#state.num_timeouts, State};
handle_call({call_reply_timeout, Timeout}, _From, State) ->
    {reply, ok, State, Timeout};
handle_call({call_noreply_timeout, Timeout}, _From, State) ->
    {noreply, State, Timeout}.

handle_cast(crash, _State) ->
    throw(test_crash);
handle_cast(ping, #state{num_casts = NumCasts} = State) ->
    {noreply, State#state{num_casts = NumCasts + 1}};
handle_cast({cast_timeout, Timeout}, State) ->
    {noreply, State, Timeout};
handle_cast({set_info_timeout, Timeout}, State) ->
    {noreply, State#state{info_timeout = Timeout}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(ping, #state{num_infos = NumInfos, info_timeout = InfoTimeout} = State) ->
    NewState = State#state{num_infos = NumInfos + 1},
    case InfoTimeout of
        none ->
            {noreply, NewState};
        Other ->
            {noreply, NewState, Other}
    end;
handle_info(timeout, #state{num_timeouts = NumTimeouts, info_timeout = InfoTimeout} = State) ->
    NewState = State#state{num_timeouts = NumTimeouts + 1},
    case InfoTimeout of
        none ->
            {noreply, NewState};
        Other ->
            {noreply, NewState, Other}
    end;
handle_info(_Info, #state{info_timeout = InfoTimeout} = State) ->
    case InfoTimeout of
        none ->
            {noreply, State};
        Other ->
            {noreply, State, Other}
    end.

terminate(_Reason, _State) ->
    ok.
