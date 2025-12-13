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
-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {
    num_casts = 0,
    num_infos = 0,
    num_timeouts = 0,
    info_timeout = none,
    crash_in_terminate = false
}).

test() ->
    ok = test_call(),
    ok = test_cast(),
    ok = test_info(),
    ok = test_start_link(),
    ok = test_start_monitor(),
    ok = test_start_name(),
    ok = test_continue(),
    ok = test_init_exception(),
    ok = test_late_reply(),
    ok = test_concurrent_clients(),
    ok = test_timeout_call(),
    ok = test_timeout_cast(),
    ok = test_timeout_info(),
    ok = test_timeout_tuple_return(),
    ok = test_register(),
    ok = test_call_unregistered(),
    ok = test_cast_unregistered(),
    ok = test_normal_exit_call(),
    ok = test_abnormal_exit_call(),
    ok = test_crash_call(),
    ok = test_crash_in_terminate(),
    ok = test_call_noproc(),
    ok = test_stop_noproc(),
    ok = test_sys_get_state_status(),
    ok = test_sys_suspend_resume(),
    ok = test_sys_change_code(),
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
    PreviousTrapExit = erlang:process_flag(trap_exit, true),
    ok = gen_server:cast(Pid, crash),
    ok =
        receive
            {'EXIT', Pid, _Reason} -> ok
        after 30000 -> timeout
        end,
    true = erlang:process_flag(trap_exit, PreviousTrapExit),
    ok.

test_start_monitor() ->
    case get_otp_version() of
        %% Test on AtomVM and OTP 23 and later
        Version when Version >= 23 ->
            {ok, {Pid, Ref}} = gen_server:start_monitor(?MODULE, [], []),

            pong = gen_server:call(Pid, ping),
            pong = gen_server:call(Pid, reply_ping),
            ok = gen_server:cast(Pid, crash),
            ok =
                receive
                    {'DOWN', Ref, process, Pid, _Reason} -> ok
                after 30000 -> timeout
                end,
            ok;
        _ ->
            ok
    end.

test_start_name() ->
    undefined = whereis(?MODULE),
    {ok, Pid1} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
    Pid1 = whereis(?MODULE),
    ok = gen_server:stop(Pid1),
    undefined = whereis(?MODULE),

    PreviousTrapExit = erlang:process_flag(trap_exit, true),
    {ok, Pid2} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Pid2 = whereis(?MODULE),
    ok = gen_server:stop(Pid2),
    normal =
        receive
            {'EXIT', Pid2, Reason} -> Reason
        after 5000 -> timeout
        end,
    undefined = whereis(?MODULE),
    true = erlang:process_flag(trap_exit, PreviousTrapExit),

    case get_otp_version() of
        %% Test on AtomVM and OTP 23 and later
        Version when Version >= 23 ->
            {ok, {Pid3, MonitorRef}} = gen_server:start_monitor({local, ?MODULE}, ?MODULE, [], []),
            Pid3 = whereis(?MODULE),
            % Demonitor to avoid any DOWN message
            true = demonitor(MonitorRef),
            ok = gen_server:stop(Pid3),
            undefined = whereis(?MODULE);
        _ ->
            ok
    end,
    ok.

test_continue() ->
    {ok, Pid} = gen_server:start_link(?MODULE, {continue, self()}, []),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    gen_server:call(Pid, {continue_reply, self()}),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    gen_server:call(Pid, {continue_noreply, self()}),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    gen_server:cast(Pid, {continue_noreply, self()}),
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    Pid ! {continue_noreply, self()},
    [{Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    Pid ! {continue_continue, self()},
    [{Pid, before_continue}, {Pid, continue}, {Pid, after_continue}] = read_replies(Pid),

    Ref = monitor(process, Pid),
    Pid ! continue_stop,
    verify_down_reason(Ref, Pid, normal).

read_replies(Pid) ->
    receive
        {Pid, ack} -> read_replies()
    after 1000 ->
        error
    end.

read_replies() ->
    receive
        Msg -> [Msg | read_replies()]
    after 0 -> []
    end.

verify_down_reason(MRef, Server, Reason) ->
    receive
        {'DOWN', MRef, process, Server, Reason} ->
            ok
    after 5000 ->
        error
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
            exit:{timeout, _Location} ->
                timeout;
            T:V ->
                erlang:display({unexpected, T, V}),
                unexpected
        end,
    %%
    %% flush the late message in the mailbox
    %% OTP's gen_server(3) man page says it's a two element tuple with a ref as
    %% a first element.
    %%
    %% Starting with OTP 24, gen_server:call uses process aliases, so late
    %% replies will not be received.
    %%
    ProcessAliases =
        case erlang:system_info(machine) of
            "BEAM" -> erlang:system_info(version) >= "12.";
            "ATOM" -> false
        end,
    ok =
        case ProcessAliases of
            true ->
                ok;
            false ->
                receive
                    {Ref, _} when is_reference(Ref) -> ok;
                    Other -> {unexpected_other, Other}
                after 1000 ->
                    timeout
                end
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
    Ref = make_ref(),
    ok = gen_server:call(Pid, {call_reply_timeout, 10, self(), Ref}),
    ok =
        receive
            {Ref, 1} -> ok
        after 5000 -> timeout
        end,
    ok = gen_server:call(Pid, {call_reply_timeout, infinity, self(), Ref}),
    ok =
        receive
            {Ref, _} -> unexpected
        after 500 -> ok
        end,
    ok =
        try
            gen_server:call(Pid, {call_noreply_timeout, 10, self(), Ref}, 100),
            unexpected
        catch
            _:_ -> ok
        end,
    ok =
        receive
            {Ref, 1} -> ok
        after 5000 -> timeout
        end,
    gen_server:stop(Pid),
    ok.

test_timeout_cast() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    0 = gen_server:call(Pid, get_num_timeouts),
    Ref = make_ref(),
    ok = gen_server:cast(Pid, {cast_timeout, 10, self(), Ref}),
    ok =
        receive
            {Ref, 1} -> ok
        after 5000 -> timeout
        end,
    ok = gen_server:cast(Pid, {cast_timeout, infinity, self(), Ref}),
    ok =
        receive
            {Ref, _} -> unexpected
        after 500 -> ok
        end,
    gen_server:stop(Pid),
    ok.

test_timeout_info() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    0 = gen_server:call(Pid, get_num_timeouts),
    Ref = make_ref(),
    ok = gen_server:cast(Pid, {set_info_timeout, 10, self(), Ref}),
    Pid ! ping,
    ok =
        receive
            {Ref, 1} -> ok
        after 5000 -> timeout
        end,
    ok =
        receive
            {Ref, 2} -> ok
        after 5000 -> timeout
        end,
    gen_server:stop(Pid).

test_timeout_tuple_return() ->
    case get_otp_version() of
        %% Test on AtomVM and OTP 28 and later
        Version when Version >= 28 ->
            Self = self(),
            %% test timeout tuple in init
            {ok, Pid0} = gen_server:start(?MODULE, {timeout, Self}, []),
            ok =
                receive
                    {reply_from_timeout, init} ->
                        ok
                after 1000 ->
                    {error, no_timeout_reply_after_init}
                end,
            gen_server:stop(Pid0),

            %% test timeout tuple in continue, and proceed through callback tests
            {ok, Pid1} = gen_server:start(?MODULE, {continue_timeout, Self}, []),
            ok =
                receive
                    {reply_from_timeout, continue} ->
                        ok
                after 1000 ->
                    {error, no_timeout_reply_after_continue}
                end,

            %% Test handle_call
            {ok, Ref} = gen_server:call(Pid1, {req_timeout_reply, Self}),
            ok =
                receive
                    {reply_from_timeout, Ref} ->
                        ok
                after 2000 ->
                    {error, no_timeout_reply_after_call}
                end,

            %% Test handle_cast
            gen_server:cast(Pid1, {req_timeout_reply, Self}),
            ok =
                receive
                    {reply_from_timeout, cast} ->
                        ok
                after 2000 ->
                    {error, no_timeout_reply_after_cast}
                end,

            %% Test handle_info
            gen_server:cast(Pid1, {request_info_timeout, Self}),
            ok =
                receive
                    {reply_from_timeout, info} ->
                        ok
                after 2000 ->
                    {error, no_info_timeout_reply}
                end,
            gen_server:stop(Pid1),

            {ok, Pid2} = gen_server:start(?MODULE, [], []),
            0 = gen_server:call(Pid2, get_num_timeouts),
            ok = gen_server:cast(Pid2, {tuple_timeout, 10, self()}),
            ok =
                receive
                    {Pid2, tuple_timeout_end} -> ok
                after 5000 -> timeout
                end,
            10 = gen_server:call(Pid2, get_num_timeouts),

            gen_server:stop(Pid2);
        _ ->
            ok
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

test_call_unregistered() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    try
        gen_server:call(?MODULE, foo),
        fail
    catch
        exit:{noproc, Location} ->
            erlang:display({noproc, Location}),
            ok;
        T:V ->
            erlang:display({T, V}),
            ok
    after
        ok = gen_server:stop(Pid)
    end.

test_cast_unregistered() ->
    ok = gen_server:cast(?MODULE, foo).

test_normal_exit_call() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    try
        gen_server:call(Pid, normal_exit),
        fail
    catch
        exit:{normal, _Location} ->
            ok
    end.

test_abnormal_exit_call() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    try
        gen_server:call(Pid, abnormal_exit),
        fail
    catch
        exit:{abnormal, _Location} ->
            ok
    end.

test_crash_call() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    try
        gen_server:call(Pid, crash_me),
        fail
    catch
        exit:{crash_me, _Location} ->
            ok
    end.

test_crash_in_terminate() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    ok = gen_server:call(Pid, crash_in_terminate),

    try
        gen_server:stop(Pid),
        fail
    catch
        exit:{crash_in_terminate, _Location} ->
            ok
    end.

test_call_noproc() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    ok = gen_server:stop(Pid),

    try
        gen_server:call(Pid, ping),
        fail
    catch
        exit:{noproc, _Location} ->
            ok
    end.

test_stop_noproc() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    ok = gen_server:stop(Pid),

    try
        gen_server:stop(Pid),
        fail
    catch
        exit:noproc ->
            ok
    end.

test_sys_get_state_status() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    #state{} = sys:get_state(Pid),
    {status, Pid, {module, gen_server}, _Extra} = sys:get_status(Pid),
    ok = gen_server:stop(Pid),
    ok.

test_sys_suspend_resume() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    ok = sys:suspend(Pid),
    ok =
        try
            gen_server:call(Pid, ping, 500),
            unexpected
        catch
            exit:{timeout, {gen_server, call, [Pid, ping, 500]}} -> ok
        end,
    ok = sys:resume(Pid),
    pong = gen_server:call(Pid, ping, 500),
    ok = gen_server:stop(Pid),
    ok.

test_sys_change_code() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    ok = gen_server:cast(Pid, ping),
    1 = gen_server:call(Pid, get_num_casts),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, ?MODULE, "old_vsn", {extra, self()}),
    ok =
        receive
            {Pid, updated, "old_vsn"} -> ok
        after 500 -> timeout
        end,
    ok = sys:resume(Pid),
    0 = gen_server:call(Pid, get_num_casts),
    ok = gen_server:stop(Pid),
    ok.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.

%%
%% callbacks
%%

init(throwme) ->
    throw(throwme);
init({continue, Pid}) ->
    io:format("init(continue) -> ~p~n", [Pid]),
    self() ! {after_continue, Pid},
    {ok, [], {continue, {message, Pid}}};
init({timeout, Pid}) ->
    {ok, [], {timeout, 1, {timeout_reply, Pid, init}}};
init({continue_timeout, Pid}) ->
    {ok, [], {continue, {timeout_reply, Pid}}};
init(_) ->
    {ok, #state{}}.

handle_continue({continue, Pid}, State) ->
    Pid ! {self(), before_continue},
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid}}};
handle_continue(stop, State) ->
    {stop, normal, State};
handle_continue({message, Pid}, State) ->
    Pid ! {self(), continue},
    {noreply, State};
handle_continue({message, Pid, From}, State) ->
    Pid ! {self(), continue},
    gen_server:reply(From, ok),
    {noreply, State};
handle_continue({timeout_reply, Pid}, State) ->
    {noreply, State, {timeout, 1, {timeout_reply, Pid, continue}}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call({continue_reply, Pid}, _From, State) ->
    self() ! {after_continue, Pid},
    {reply, ok, State, {continue, {message, Pid}}};
handle_call({continue_noreply, Pid}, From, State) ->
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid, From}}};
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
handle_call({call_reply_timeout, Timeout, Client, Ref}, _From, State) ->
    {reply, ok, State#state{info_timeout = {infinity, Client, Ref, 0}}, Timeout};
handle_call({call_noreply_timeout, Timeout, Client, Ref}, _From, State) ->
    {noreply, State#state{info_timeout = {infinity, Client, Ref, 0}}, Timeout};
handle_call(normal_exit, _From, State) ->
    {stop, normal, State};
handle_call(abnormal_exit, _From, State) ->
    {stop, abnormal, State};
handle_call(crash_me, _From, State) ->
    exit(crash_me),
    {reply, noop, State};
handle_call(crash_in_terminate, _From, State) ->
    {reply, ok, State#state{crash_in_terminate = true}};
handle_call({req_timeout_reply, Replyto}, _From, State) ->
    Ref = make_ref(),
    {reply, {ok, Ref}, State, {timeout, 1, {timeout_reply, Replyto, Ref}}}.

handle_cast({continue_noreply, Pid}, State) ->
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid}}};
handle_cast(crash, _State) ->
    throw(test_crash);
handle_cast(ping, #state{num_casts = NumCasts} = State) ->
    {noreply, State#state{num_casts = NumCasts + 1}};
handle_cast({cast_timeout, Timeout, Client, Ref}, State) ->
    {noreply, State#state{info_timeout = {infinity, Client, Ref, 0}}, Timeout};
handle_cast({tuple_timeout, Timeouts, Caller}, State) ->
    {noreply, State, {timeout, 1, {do_tuple_timeouts, Timeouts, Caller}}};
handle_cast({set_info_timeout, Timeout, Client, Ref}, State) ->
    {noreply, State#state{info_timeout = {Timeout, Client, Ref, 0}}};
handle_cast({req_timeout_reply, Pid}, State) ->
    {noreply, State, {timeout, 1, {timeout_reply, Pid, cast}}};
handle_cast({request_info_timeout, Pid}, State) ->
    {noreply, State, {timeout, 1, {request_info_timeout, Pid}}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({after_continue, Pid}, State) ->
    Pid ! {self(), after_continue},
    Pid ! {self(), ack},
    {noreply, State};
handle_info(continue_stop, State) ->
    {noreply, State, {continue, stop}};
handle_info({continue_noreply, Pid}, State) ->
    self() ! {after_continue, Pid},
    {noreply, State, {continue, {message, Pid}}};
handle_info({continue_continue, Pid}, State) ->
    {noreply, State, {continue, {continue, Pid}}};
handle_info(ping, #state{num_infos = NumInfos, info_timeout = InfoTimeout} = State) ->
    NewState = State#state{num_infos = NumInfos + 1},
    case InfoTimeout of
        none ->
            {noreply, NewState};
        {Timeout, _Client, _Ref, _Count} ->
            {noreply, NewState, Timeout}
    end;
handle_info(timeout, #state{num_timeouts = NumTimeouts, info_timeout = InfoTimeout} = State) ->
    NewState = State#state{num_timeouts = NumTimeouts + 1},
    case InfoTimeout of
        none ->
            {noreply, NewState};
        {Timeout, Client, Ref, Count} ->
            NewCount = Count + 1,
            Client ! {Ref, NewCount},
            {noreply, NewState#state{info_timeout = {Timeout, Client, Ref, NewCount}}, Timeout}
    end;
handle_info({timeout_reply, From, Tag} = _Info, State) ->
    From ! {reply_from_timeout, Tag},
    {noreply, State};
handle_info({request_info_timeout, From}, State) ->
    {noreply, State, {timeout, 1, {info_timeout_reply, From}}};
handle_info({info_timeout_reply, From}, State) ->
    From ! {reply_from_timeout, info},
    {noreply, State};
handle_info({do_tuple_timeouts, 0, Caller}, State) ->
    Caller ! {self(), tuple_timeout_end},
    {noreply, State};
handle_info({do_tuple_timeouts, Timeouts, Caller}, #state{num_timeouts = TimeoutCt} = State) ->
    {noreply, State#state{num_timeouts = TimeoutCt + 1},
        {timeout, 1, {do_tuple_timeouts, Timeouts - 1, Caller}}};
handle_info(_Info, #state{info_timeout = InfoTimeout} = State) ->
    case InfoTimeout of
        none ->
            {noreply, State};
        Other ->
            {noreply, State, Other}
    end.

code_change(OldVsn, State, {extra, Pid}) ->
    Pid ! {self(), updated, OldVsn},
    {ok, State#state{num_casts = 0}}.

terminate(_Reason, #state{crash_in_terminate = true} = _State) ->
    error(crash_in_terminate);
terminate(_Reason, _State) ->
    ok.
