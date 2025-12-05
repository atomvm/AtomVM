%
% This file is part of AtomVM.
%
% Copyright 2021 Davide Bettio <davide@uninstall.it>
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
%% @doc An implementation of the Erlang/OTP supervisor interface.
%%
%% This module implements a strict subset of the Erlang/OTP supervisor
%% interface, supporting operations for local creation and management of
%% supervisor instances.
%%
%% This module is designed to be API-compatible with supervisor, with exceptions
%%  noted below.
%%
%% Caveats:
%% <ul>
%%     <li>Support only for locally named procs</li>
%%     <li>No support for simple_one_for_one or one_for_rest strategies</li>
%%     <li>No support for hibernate</li>
%%     <li>No support for automatic shutdown</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(supervisor).

-behavior(gen_server).

-export([
    start_link/2,
    start_link/3,
    start_child/2,
    terminate_child/2,
    restart_child/2,
    delete_child/2,
    which_children/1,
    count_children/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-export_type([
    child_spec/0,
    startchild_ret/0,
    startlink_ret/0,
    startlink_err/0,
    strategy/0,
    sup_flags/0,
    sup_name/0,
    sup_ref/0
]).

-type restart() ::
    permanent
    | transient
    | temporary
    | {terminating, permanent | transient | temporary, gen_server:from()}.
-type shutdown() :: brutal_kill | timeout().
-type worker() :: worker | supervisor.
-type child_id() :: term().
-type child() :: undefined | pid().

-type strategy() :: one_for_all | one_for_one.
-type sup_flags() ::
    #{
        strategy => strategy(),
        intensity => non_neg_integer(),
        period => pos_integer()
    }
    | {RestartStrategy :: strategy(), Intensity :: non_neg_integer(), Period :: pos_integer()}.

-type child_spec() ::
    #{
        id := any(),
        start := {module(), atom(), [any()]},
        restart => restart(),
        shutdown => shutdown(),
        type => worker(),
        modules => [module()] | dynamic
    }
    | {
        Id :: any(),
        StartFunc :: {module(), atom(), [any()]},
        Restart :: restart(),
        Shutdown :: shutdown(),
        Type :: worker(),
        Modules :: [module()] | dynamic
    }.

-type startlink_ret() :: {ok, pid()} | ignore | {error, startlink_err()}.
-type startlink_err() :: {already_started, pid()} | {shutdown, term()} | term().
-type startchild_ret() ::
    {ok, Child :: child()} | {ok, Child :: child(), Info :: term()} | {error, startchild_err()}.
-type startchild_err() :: already_present | {already_started, Child :: child()} | term().
-type sup_name() :: {local, Name :: atom()}.
-type sup_ref() ::
    (Name :: atom())
    | {Name :: atom(), Node :: node()}
    | pid().

-record(child, {
    pid = undefined :: pid() | undefined | {restarting, pid()} | {restarting, undefined},
    id :: any(),
    start :: {module(), atom(), [any()] | undefined},
    restart :: restart(),
    shutdown :: shutdown(),
    type :: worker(),
    modules = [] :: [module()] | dynamic
}).

-define(DEFAULT_INTENSITY, 1).
-define(DEFAULT_PERIOD, 5).
%% note: the list of children should always be kept in order, with last to start at the head.
-record(state, {
    restart_strategy = one_for_one :: strategy(),
    intensity = ?DEFAULT_INTENSITY :: non_neg_integer(),
    period = ?DEFAULT_PERIOD :: pos_integer(),
    restart_count = 0 :: non_neg_integer(),
    restarts = [] :: [integer()],
    children = [] :: [#child{}]
}).

%% Used to trim stale restarts when the 'intensity' value is large.
%% The number of restarts before triggering a purge of restarts older
%% than 'period', so stale restarts do not continue to consume ram for
%% the sake of MCUs with limited memory. In the future a function
%% could be used to set a sane default for the platform (OTP uses 1000).
-define(STALE_RESTART_LIMIT, 100).

-spec start_link(Module :: module(), Args :: [any()]) -> startlink_ret().
start_link(Module, Args) ->
    gen_server:start_link(?MODULE, {Module, Args}, []).

-spec start_link(SupName :: sup_name(), Module :: module(), Args :: [any()]) -> startlink_ret().
start_link(SupName, Module, Args) ->
    gen_server:start_link(SupName, ?MODULE, {Module, Args}, []).

-spec start_child(Supervisor :: sup_ref(), ChildSpec :: child_spec()) -> startchild_ret().
start_child(Supervisor, ChildSpec) ->
    gen_server:call(Supervisor, {start_child, ChildSpec}).

-spec terminate_child(Supervisor :: sup_ref(), ChildId :: any()) -> ok | {error, not_found}.
terminate_child(Supervisor, ChildId) ->
    gen_server:call(Supervisor, {terminate_child, ChildId}).

-spec restart_child(Supervisor :: sup_ref(), ChildId :: any()) ->
    {ok, Child :: child()}
    | {ok, Child :: child(), Info :: term()}
    | {error, Reason :: running | restarting | not_found | term()}.
restart_child(Supervisor, ChildId) ->
    gen_server:call(Supervisor, {restart_child, ChildId}).

-spec delete_child(Supervisor :: sup_ref(), ChildId :: any()) ->
    ok | {error, Reason :: running | restarting | not_found}.
delete_child(Supervisor, ChildId) ->
    gen_server:call(Supervisor, {delete_child, ChildId}).

-spec which_children(Supervisor :: sup_ref()) ->
    [
        {
            Id :: child_id() | undefined,
            Child :: child() | restarting,
            Type :: worker(),
            Modules :: [module()]
        }
    ].
which_children(Supervisor) ->
    gen_server:call(Supervisor, which_children).

-spec count_children(Supervisor :: sup_ref()) ->
    [
        {specs, ChildSpecCount :: non_neg_integer()}
        | {active, ActiveProcessCount :: non_neg_integer()}
        | {supervisors, ChildSupervisorCount :: non_neg_integer()}
        | {workers, ChildWorkerCount :: non_neg_integer()}
    ].
count_children(Supervisor) ->
    gen_server:call(Supervisor, count_children).

% @hidden
-spec init({Mod :: module(), Args :: [any()]}) ->
    {ok, State :: #state{}} | {stop, {bad_return, {Mod :: module(), init, Reason :: term()}}}.
init({Mod, Args}) ->
    erlang:process_flag(trap_exit, true),
    case Mod:init(Args) of
        {ok, {{Strategy, Intensity, Period}, StartSpec}} ->
            State = init_state(StartSpec, #state{
                restart_strategy = Strategy,
                intensity = Intensity,
                period = Period
            }),
            NewChildren = start_children(State#state.children, []),
            {ok, State#state{children = NewChildren}};
        {ok, {#{} = SupSpec, StartSpec}} ->
            Strategy = maps:get(strategy, SupSpec, one_for_one),
            Intensity = maps:get(intensity, SupSpec, ?DEFAULT_INTENSITY),
            Period = maps:get(period, SupSpec, ?DEFAULT_PERIOD),
            State = init_state(StartSpec, #state{
                restart_strategy = Strategy,
                intensity = Intensity,
                period = Period
            }),
            NewChildren = start_children(State#state.children, []),
            {ok, State#state{children = NewChildren}};
        Error ->
            % TODO: log supervisor init failure
            {stop, {bad_return, {Mod, init, Error}}}
    end.

-spec child_spec_to_record(child_spec()) -> #child{}.
child_spec_to_record({ChildId, MFA, Restart, Shutdown, Type, Modules}) ->
    #child{
        id = ChildId,
        start = MFA,
        restart = Restart,
        shutdown = Shutdown,
        type = Type,
        modules = Modules
    };
child_spec_to_record(#{id := ChildId, start := MFA} = ChildMap) ->
    Restart = maps:get(restart, ChildMap, permanent),
    Type = maps:get(type, ChildMap, worker),
    Shutdown = maps:get(
        shutdown,
        ChildMap,
        case Type of
            worker -> 5000;
            supervisor -> infinity
        end
    ),
    Modules = maps:get(
        modules,
        ChildMap,
        case MFA of
            {M, _, _} -> [M];
            _ -> []
        end
    ),
    #child{
        id = ChildId,
        start = MFA,
        restart = Restart,
        shutdown = Shutdown,
        type = Type,
        modules = Modules
    }.

% @hidden
-spec init_state(ChildSpecs :: [child_spec()], State :: #state{}) -> State :: #state{}.
init_state([ChildSpec | T], State) ->
    Child = child_spec_to_record(ChildSpec),
    NewChildren = [Child | State#state.children],
    init_state(T, State#state{children = NewChildren});
init_state([], State) ->
    State#state{children = lists:reverse(State#state.children)}.

-spec start_children(ChildSpecs :: [#child{}], [#child{}]) ->
    Childs :: [#child{}].
start_children([Child | T], StartedC) ->
    case try_start(Child) of
        {ok, Pid, _Result} ->
            start_children(T, [Child#child{pid = Pid} | StartedC])
    end;
start_children([], StartedC) ->
    StartedC.

% @hidden
handle_call({start_child, ChildSpec}, _From, #state{children = Children} = State) ->
    Child = child_spec_to_record(ChildSpec),
    #child{id = ID} = Child,
    case lists:keyfind(ID, #child.id, State#state.children) of
        #child{pid = undefined} ->
            {reply, {error, already_present}, State};
        #child{pid = {restarting, _Pid}} ->
            {reply, {error, restarting}, State};
        #child{pid = Pid} ->
            {reply, {error, {already_started, Pid}}, State};
        false ->
            case try_start(Child) of
                {ok, Pid, Result} ->
                    UpdatedChild = Child#child{pid = Pid},
                    %% The last child to start should always be at the end of the child
                    %% start list.
                    {reply, Result, State#state{children = [UpdatedChild | Children]}};
                {error, _Reason} = ErrorT ->
                    {reply, ErrorT, State}
            end
    end;
handle_call({terminate_child, ID}, From, #state{children = Children} = State) ->
    case lists:keyfind(ID, #child.id, Children) of
        #child{pid = undefined} ->
            {reply, ok, State};
        #child{restart = Restart} = Child ->
            do_terminate(Child),
            NewChild = Child#child{restart = {terminating, Restart, From}},
            NewChildren = lists:keyreplace(ID, #child.id, Children, NewChild),
            {noreply, State#state{children = NewChildren}};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call({restart_child, ID}, _From, #state{children = Children} = State) ->
    case lists:keyfind(ID, #child.id, Children) of
        #child{pid = undefined} = Child ->
            case try_start(Child) of
                {ok, NewPid, Result} ->
                    NewChild = Child#child{pid = NewPid},
                    NewChildren = lists:keyreplace(
                        ID, #child.id, Children, NewChild
                    ),
                    {reply, Result, State#state{children = NewChildren}};
                {error, _Reason} = ErrorT ->
                    {reply, ErrorT, State}
            end;
        #child{pid = {restarting, _}} ->
            {reply, {error, restarting}, State};
        #child{} ->
            {reply, {error, running}, State};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call({delete_child, ID}, _From, #state{children = Children} = State) ->
    case lists:keyfind(ID, #child.id, Children) of
        #child{pid = undefined} ->
            NewChildren = lists:keydelete(ID, #child.id, Children),
            {reply, ok, State#state{children = NewChildren}};
        #child{pid = {restarting, _}} ->
            {reply, {error, restarting}, State};
        #child{} ->
            {reply, {error, running}, State};
        false ->
            {reply, {error, not_found}, State}
    end;
handle_call(which_children, _From, #state{children = Children} = State) ->
    ChildrenInfo = lists:map(fun child_to_info/1, Children),
    {reply, ChildrenInfo, State};
handle_call(count_children, _From, #state{children = Children} = State) ->
    {Specs, Active, Supers, Workers} =
        lists:foldl(fun count_child/2, {0, 0, 0, 0}, Children),
    Reply = [{specs, Specs}, {active, Active}, {supervisors, Supers}, {workers, Workers}],
    {reply, Reply, State}.

% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

% @hidden
handle_info({'EXIT', Pid, Reason}, State) ->
    % TODO: log crash report
    handle_child_exit(Pid, Reason, State);
handle_info({ensure_killed, Pid}, State) ->
    case lists:keyfind(Pid, #child.pid, State#state.children) of
        false ->
            {noreply, State};
        #child{} ->
            exit(Pid, kill),
            {noreply, State}
    end;
handle_info({restart_many_children, []}, State) ->
    {noreply, State};
handle_info(
    {restart_many_children, [#child{pid = {restarting, _Pid0} = Pid} = Child | Children]}, State
) ->
    case try_start(Child) of
        {ok, NewPid, _Result} ->
            NewChild = Child#child{pid = NewPid},
            NewChildren = lists:keyreplace(
                Pid, #child.pid, State#state.children, NewChild
            ),
            {noreply, State#state{children = NewChildren},
                {timeout, 0, {restart_many_children, Children}}};
        {error, Reason} ->
            handle_child_exit(Pid, {restart, Reason}, State)
    end;
handle_info({try_again_restart, Id}, State) ->
    case lists:keyfind(Id, #child.id, State#state.children) of
        false ->
            {noreply, State};
        Child ->
            case add_restart(State) of
                {ok, State1} ->
                    case try_start(Child) of
                        {ok, NewPid, _Result} ->
                            UpdatedChildren = lists:keyreplace(
                                Id, #child.id, State1#state.children, Child#child{pid = NewPid}
                            ),
                            {noreply, State1#state{children = UpdatedChildren}};
                        {error, {_, _}} ->
                            % TODO: log crash report
                            {noreply, State1, {timeout, 0, {try_again_restart, Id}}}
                    end;
                {shutdown, State1} ->
                    RemainingChildren = lists:keydelete(Id, #child.id, State1#state.children),
                    % TODO: log supervisor shutdown
                    {stop, shutdown, State1#state{children = RemainingChildren}}
            end
    end;
handle_info({restart_many_children, [#child{pid = undefined} = _Child | Children]}, State) ->
    {noreply, State, {timeout, 0, {restart_many_children, Children}}};
handle_info(_Msg, State) ->
    %TODO: log unexpected message to debug
    {noreply, State}.

%% @hidden
terminate(_Reason, #state{children = Children} = State) ->
    %% Shutdown children last to first.
    RemainingChildren = loop_terminate(Children, []),
    loop_wait_termination(RemainingChildren),
    {ok, State}.

%% Internal Private Functions

%% @private
handle_child_exit(Pid, Reason, State) ->
    case lists:keyfind(Pid, #child.pid, State#state.children) of
        false ->
            {noreply, State};
        #child{restart = {terminating, temporary, From}} ->
            gen_server:reply(From, ok),
            NewChildren = lists:keydelete(Pid, #child.pid, State#state.children),
            {noreply, State#state{children = NewChildren}};
        #child{restart = {terminating, Restart, From}} = Child ->
            gen_server:reply(From, ok),
            NewChildren = lists:keyreplace(Pid, #child.pid, State#state.children, Child#child{
                pid = undefined, restart = Restart
            }),
            {noreply, State#state{children = NewChildren}};
        #child{} = Child ->
            case should_restart(Reason, Child#child.restart) of
                true ->
                    case add_restart(State) of
                        {ok, State1} ->
                            handle_restart_strategy(Child, State1);
                        {shutdown, State1} ->
                            RemainingChildren = lists:keydelete(
                                Pid, #child.pid, State1#state.children
                            ),
                            % TODO: log supervisor shutdown
                            {stop, shutdown, State1#state{children = RemainingChildren}}
                    end;
                false ->
                    Children = lists:keydelete(Pid, #child.pid, State#state.children),
                    {noreply, State#state{children = Children}}
            end
    end.

handle_restart_strategy(
    #child{id = Id} = Child, #state{restart_strategy = one_for_one} = State
) ->
    case try_start(Child) of
        {ok, NewPid, _Result} ->
            NewChild = Child#child{pid = NewPid},
            Children = lists:keyreplace(
                Id, #child.id, State#state.children, NewChild
            ),
            {noreply, State#state{children = Children}};
        {error, _} ->
            NewChild = Child#child{pid = {restarting, Child#child.pid}},
            Children = lists:keyreplace(
                Id, #child.id, State#state.children, NewChild
            ),
            {noreply, State#state{children = Children}, {timeout, 0, {try_again_restart, Id}}}
    end;
handle_restart_strategy(
    #child{pid = Pid} = Child, #state{restart_strategy = one_for_all} = State
) ->
    Children =
        case Pid of
            {restarting, _} ->
                State#state.children;
            Pid when is_pid(Pid) ->
                lists:keyreplace(Pid, #child.pid, State#state.children, Child#child{
                    pid = {restarting, Pid}
                })
        end,
    ok = terminate_one_for_all(Children),
    {ok, NewChildren} = get_restart_children(Children),
    %% NewChildren is startup order (first at head) and needs to be reversed to keep Children in correct order in #state{}
    {noreply, State#state{children = lists:reverse(NewChildren)},
        {timeout, 0, {restart_many_children, NewChildren}}}.

should_restart(_Reason, permanent) ->
    true;
should_restart(_Reason, temporary) ->
    false;
should_restart(Reason, transient) ->
    case Reason of
        normal -> false;
        _any -> true
    end.

loop_terminate([#child{pid = undefined} | Tail], AccRemaining) ->
    loop_terminate(Tail, AccRemaining);
loop_terminate([#child{pid = {restarting, _}} | Tail], AccRemaining) ->
    loop_terminate(Tail, AccRemaining);
loop_terminate([#child{pid = Pid} = Child | Tail], AccRemaining) when is_pid(Pid) ->
    do_terminate(Child),
    loop_terminate(Tail, [Pid | AccRemaining]);
loop_terminate([], AccRemaining) ->
    AccRemaining.

loop_wait_termination([]) ->
    ok;
loop_wait_termination(RemainingChildren0) ->
    receive
        {'EXIT', Pid, _Reason} ->
            RemainingChildren1 = lists:delete(Pid, RemainingChildren0),
            loop_wait_termination(RemainingChildren1);
        {ensure_killed, Pid} ->
            case lists:member(Pid, RemainingChildren0) of
                true ->
                    exit(Pid, kill),
                    loop_wait_termination(RemainingChildren0);
                false ->
                    loop_wait_termination(RemainingChildren0)
            end
    end.

try_start(#child{start = {M, F, Args}} = Record) ->
    try
        case apply(M, F, Args) of
            {ok, Pid} when is_pid(Pid) ->
                {ok, Pid, {ok, Pid}};
            {ok, Pid, Info} when is_pid(Pid) ->
                {ok, Pid, {ok, Pid, Info}};
            ignore ->
                {ok, undefined, {ok, undefined}};
            {error, Reason} ->
                {error, {Reason, Record}};
            Other ->
                {error, {Other, Record}}
        end
    catch
        error:Error ->
            {error, {{'EXIT', Error}, Record}}
    end.

get_restart_children(Children) ->
    get_restart_children(Children, []).

get_restart_children([], NewChildren) ->
    {ok, NewChildren};
get_restart_children([Child | Children], NewChildren) ->
    case Child of
        #child{restart = {terminating, temporary, _From}} ->
            get_restart_children(Children, NewChildren);
        #child{restart = {terminating, _Restart, _From}} ->
            get_restart_children(Children, [Child | NewChildren]);
        #child{pid = undefined, restart = temporary} ->
            get_restart_children(Children, NewChildren);
        #child{pid = undefined} = Child ->
            get_restart_children(Children, [Child | NewChildren]);
        #child{pid = {restarting, _Pid}} = Child ->
            get_restart_children(Children, [Child | NewChildren]);
        #child{pid = Pid, restart = temporary} = Child when is_pid(Pid) ->
            get_restart_children(Children, NewChildren);
        #child{pid = Pid} = Child when is_pid(Pid) ->
            get_restart_children(Children, [Child#child{pid = {restarting, Pid}} | NewChildren])
    end.

terminate_one_for_all(Children) ->
    %% Always shut down last child first
    do_terminate_one_for_all(Children, []).

do_terminate_one_for_all([], StopPids) ->
    ok = loop_wait_termination(StopPids),
    %% After accumulation NewChildren are in correct order for restart.
    ok;
do_terminate_one_for_all([Child | Children], StopPids) ->
    case Child of
        #child{pid = Pid} = Child when is_pid(Pid) ->
            do_terminate(Child),
            do_terminate_one_for_all(Children, [Pid | StopPids]);
        #child{pid = undefined} ->
            do_terminate_one_for_all(Children, StopPids);
        #child{pid = {restarting, _Pid}} = Child ->
            do_terminate_one_for_all(Children, StopPids)
    end.

add_restart(
    #state{
        intensity = Intensity, period = Period, restart_count = RestartCount, restarts = Restarts
    } = State
) ->
    Now = erlang:monotonic_time(millisecond),
    Threshold = Now - Period * 1000,
    case can_restart(Intensity, Threshold, Restarts, RestartCount) of
        {true, RestartCount1, Restarts1} ->
            {ok, State#state{
                restarts = Restarts1 ++ [Now], restart_count = RestartCount1 + 1
            }};
        {false, _RestartCount1, _Restarts1} ->
            % TODO: log supervisor shutdown due to maximum intensity exceeded
            {shutdown, State}
    end.

can_restart(0, _, _, _) ->
    {false, 0, []};
can_restart(_, _, _, 0) ->
    {true, 0, []};
can_restart(Intensity, Threshold, Restarts, RestartCount) when
    RestartCount >= ?STALE_RESTART_LIMIT
->
    {NewCount, Restarts1} = trim_expired_restarts(Threshold, lists:sort(Restarts)),
    can_restart(Intensity, Threshold, Restarts1, NewCount);
can_restart(Intensity, Threshold, [Restart | _] = Restarts, RestartCount) when
    RestartCount >= Intensity andalso Restart < Threshold
->
    {NewCount, Restarts1} = trim_expired_restarts(Threshold, lists:sort(Restarts)),
    can_restart(Intensity, Threshold, Restarts1, NewCount);
can_restart(Intensity, _, Restarts, RestartCount) when RestartCount >= Intensity ->
    {false, RestartCount, Restarts};
can_restart(Intensity, _, Restarts, RestartCount) when RestartCount < Intensity ->
    {true, RestartCount, Restarts}.

trim_expired_restarts(Threshold, [Restart | Restarts]) when Restart < Threshold ->
    trim_expired_restarts(Threshold, Restarts);
trim_expired_restarts(_Threshold, Restarts) ->
    {length(Restarts), Restarts}.

child_to_info(#child{id = Id, pid = Pid, type = Type, modules = Modules}) ->
    Child =
        case Pid of
            undefined -> undefined;
            {restarting, _} -> restarting;
            _ when is_pid(Pid) -> Pid
        end,
    {Id, Child, Type, Modules}.

count_child(#child{pid = Pid, type = worker}, {Specs, Active, Supers, Workers}) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true -> {Specs + 1, Active + 1, Supers, Workers + 1};
        false -> {Specs + 1, Active, Supers, Workers + 1}
    end;
count_child(#child{pid = Pid, type = supervisor}, {Specs, Active, Supers, Workers}) ->
    case is_pid(Pid) andalso is_process_alive(Pid) of
        true -> {Specs + 1, Active + 1, Supers + 1, Workers};
        false -> {Specs + 1, Active, Supers + 1, Workers}
    end.

do_terminate(#child{pid = Pid, shutdown = brutal_kill}) ->
    exit(Pid, kill);
do_terminate(#child{pid = Pid, shutdown = infinity}) ->
    exit(Pid, shutdown);
do_terminate(#child{pid = Pid, shutdown = Timeout}) when is_integer(Timeout) ->
    exit(Pid, shutdown),
    erlang:send_after(Timeout, self(), {ensure_killed, Pid}).
