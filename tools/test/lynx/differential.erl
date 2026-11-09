% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(differential).
-export([
    compare/2,
    arithmetic/3,
    maps/3,
    bits/4,
    float_bits/4,
    call/3,
    map_entries/1,
    map_fold/2,
    dictionary/2,
    float_text/2,
    short_circuit/3,
    receive_timeout/2,
    receive_guards/3,
    send_destination/3,
    process_exchange/1,
    monitor_exit/1,
    monitor_target/3,
    monitor_rebind/3,
    self_link/3,
    relink/5,
    unlink_exit_race/2,
    crossed_unlink/4,
    linked_exit/2,
    exit_signal/2,
    exit_signal/3,
    self_exit/4,
    trap_transition/3,
    monitor_options/2,
    alias_lifetime/2,
    alias_owner/2,
    target_cancel/3,
    monitor_pair/3,
    termination_queue/1,
    termination_cascade/2,
    suspended_receive/1,
    missing_link/1,
    process_liveness/0,
    raise_exception/3,
    trace_depth/3,
    trace_depth_a/1,
    trace_depth_b/1,
    function_apply/3,
    function_named/3,
    function_make/4,
    function_spawn/3,
    function_zero/0,
    function_one/1,
    function_two/2,
    function_child/0,
    function_child_error/0,
    function_child_throw/0,
    function_child_exit/0,
    utf/3,
    utf_construct/3
]).

%% Inputs cross a module boundary to prevent constant-folded operations.
call(Module, Function, Args) -> apply(Module, Function, Args).

compare(A, B) ->
    {A == B, A /= B, A =:= B, A =/= B, A < B, A =< B, A > B, A >= B}.

arithmetic(add, A, B) -> A + B;
arithmetic(sub, A, B) -> A - B;
arithmetic(mul, A, B) -> A * B;
arithmetic(idiv, A, B) -> A div B;
arithmetic(mod, A, B) -> A rem B;
arithmetic(shl, A, B) -> A bsl B;
arithmetic(shr, A, B) -> A bsr B;
arithmetic(band_op, A, B) -> A band B;
arithmetic(bxor_op, A, B) -> A bxor B;
arithmetic(fdiv, A, B) -> A / B.

maps(A, B, Size) ->
    Base = fill(Size, #{}),
    M = Base#{A => first, B => second},
    {M, map_size(M), maps:find(A, M), maps:find(B, M), maps:remove(A, M), M#{B := updated},
        compare(#{A => value}, #{B => value}), compare(#{value => A}, #{value => B})}.

fill(0, Map) -> Map;
fill(N, Map) -> fill(N - 1, Map#{N => N * 3}).

%% Enumeration order is unspecified; retain bindings and detect duplicates.
map_entries(Map) ->
    Entries = maps:to_list(Map),
    {length(Entries), maps:from_list(Entries)}.

map_fold(Map, valid) -> maps:fold(fun(K, V, Acc) -> Acc#{K => V} end, #{}, Map);
map_fold(Map, wrong_arity) -> maps:fold(fun(K, V) -> {K, V} end, #{}, Map).

dictionary(A, B) ->
    erase(),
    Empty = {get(), get(missing), get_keys(), erase()},
    First = put(A, first),
    Previous = put(A, replaced),
    put(B, second),
    Found = {get(A), get(B)},
    Entries = get(),
    Keys = get_keys(),
    Selected = get_keys(second),
    Erased = erase(A),
    Rest = erase(),
    {Empty, First, Previous, Found, length(Entries), maps:from_list(Entries),
        maps:from_list([{K, present} || K <- Keys]),
        maps:from_list([{K, present} || K <- Selected]), Erased, maps:from_list(Rest), get()}.

float_text(Value, Options) ->
    Text = float_to_binary(Value, Options),
    {Text, <<(binary_to_float(Text))/float>>}.

short_circuit(Op, Left, Right) ->
    erase(),
    Result =
        try
            put(left, seen),
            case Op of
                or_else -> Left orelse rhs(Right);
                and_also -> Left andalso rhs(Right)
            end
        catch
            C:R -> {exception, C, R}
        end,
    {Result, get(left), get(rhs)}.

rhs(value) ->
    put(rhs, seen),
    7;
rhs(throw) ->
    put(rhs, seen),
    throw(boom).

receive_timeout(Timeout, Messages) ->
    [self() ! M || M <- Messages],
    Result =
        try
            receive
                wanted -> matched
            after Timeout -> timeout
            end
        catch
            C:R -> {exception, C, R}
        end,
    {Result, drain()}.

receive_guards(Kind, Timeout, Messages) ->
    isolated(fun() ->
        [self() ! M || M <- Messages],
        First = receive_guard(Kind, Timeout),
        Next =
            case Kind of
                boolean -> false;
                tuple -> {resume, wanted};
                map -> #{key => wanted, phase => resumed};
                division -> 2;
                binary -> <<2, 7>>;
                alternatives -> fallback
            end,
        self() ! Next,
        Second = receive_guard(Kind, Timeout),
        {First, Second, drain()}
    end).

receive_guard(Kind, Timeout) ->
    receive
        M when
            Kind =:= boolean, not M;
            Kind =:= tuple, element(2, M) =:= wanted;
            Kind =:= map, map_get(key, M) =:= wanted;
            Kind =:= division, 12 div M >= 3;
            Kind =:= binary, binary_part(M, 0, 1) =:= <<2>>;
            Kind =:= alternatives, is_tuple(M), element(2, M) =:= wanted;
            Kind =:= alternatives, M =:= fallback
        ->
            {matched, M}
    after Timeout -> timeout
    end.

send_destination(Operation, Mode, Payload) ->
    isolated(fun() ->
        Parent = self(),
        {Child, Fence} = spawn_monitor(fun() ->
            receive
                {payload, Value} ->
                    Parent ! {self(), Value},
                    receive
                        stop -> ok
                    end;
                stop ->
                    ok
            end
        end),
        Target =
            case Mode of
                self ->
                    self();
                live ->
                    Child;
                dead ->
                    Child ! stop,
                    receive
                        {'DOWN', Fence, process, Child, normal} -> Child
                    end;
                named_self ->
                    true = register(lynx_send_target, self()),
                    lynx_send_target;
                named_live ->
                    true = register(lynx_send_target, Child),
                    lynx_send_target;
                missing ->
                    lynx_send_missing;
                unregistered ->
                    true = register(lynx_send_target, Child),
                    true = unregister(lynx_send_target),
                    lynx_send_target;
                reference ->
                    make_ref();
                stale_alias ->
                    Alias = erlang:alias(),
                    true = erlang:unalias(Alias),
                    Alias;
                {invalid, Value} ->
                    Value
            end,
        self() ! retained,
        Message = {payload, Payload},
        Result =
            try
                Returned =
                    case Operation of
                        operator -> Target ! Message;
                        function -> call(erlang, send, [Target, Message])
                    end,
                {sent, Returned =:= Message}
            catch
                C:R -> {exception, C, R}
            end,
        Delivery =
            case {Result, Mode} of
                {{sent, _}, M} when M =:= self; M =:= named_self ->
                    receive
                        {payload, Payload} -> {delivered, Payload}
                    end;
                {{sent, _}, M} when M =:= live; M =:= named_live ->
                    receive
                        {Child, Payload} -> {delivered, Payload}
                    end;
                _ ->
                    none
            end,
        case Mode of
            dead ->
                ok;
            _ ->
                Child ! stop,
                receive
                    {'DOWN', Fence, process, Child, normal} -> ok
                end
        end,
        {Result, Delivery, [
            case M of
                retained -> retained;
                _ -> unexpected
            end
         || M <- drain()
        ]}
    end).

drain() ->
    receive
        M -> [M | drain()]
    after 0 -> []
    end.

process_exchange(Value) ->
    First = make_ref(),
    put(key, First),
    self() ! pending,
    Parent = self(),
    {Child, Monitor} = spawn_monitor(fun() ->
        Initial = get(key),
        put(key, Value),
        receive
            {go, P} when P =:= self() ->
                Parent ! {self(), Initial, get(key), make_ref()}
        end
    end),
    Second = make_ref(),
    Child ! {go, Child},
    receive
        {Child, Initial, ChildValue, ChildRef} ->
            receive
                {'DOWN', Monitor, process, Child, Reason} ->
                    {
                        is_reference(First),
                        is_reference(ChildRef),
                        First =/= Second,
                        First =/= ChildRef,
                        Second =/= ChildRef,
                        get(key) =:= First,
                        Initial,
                        ChildValue,
                        Reason,
                        drain()
                    }
            end
    end.

monitor_exit(Mode) ->
    {Child, Monitor} = spawn_monitor(fun() ->
        receive
            go ->
                case Mode of
                    raised -> error(boom);
                    _ -> ok
                end
        end
    end),
    Child ! go,
    receive
        {'DOWN', Monitor, process, Child, Reason} ->
            case Mode of
                raised ->
                    {boom, Stack} = Reason,
                    {boom, is_list(Stack)};
                normal ->
                    Reason;
                noproc ->
                    Ref = monitor(process, Child),
                    receive
                        {'DOWN', Ref, process, Child, Missing} -> Missing
                    end;
                flush ->
                    Ref = monitor(process, Child),
                    true = demonitor(Ref, [flush]),
                    receive
                        {'DOWN', Ref, process, Child, _} -> unexpected
                    after 0 -> flushed
                    end
            end
    end.

%% Linked crashes run in a monitored worker, never in the host reference task.
isolated(Fun) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() -> Parent ! {self(), Fun()} end),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            receive
                {Pid, Result} -> {ok, Result}
            end;
        {'DOWN', Ref, process, Pid, Reason} ->
            {exit, Reason}
    end.

monitor_target(Type, State, Options) ->
    isolated(fun() ->
        {Child, Fence} = spawn_monitor(fun() ->
            receive
                stop -> ok
            end
        end),
        Target =
            case State of
                self ->
                    self();
                live ->
                    Child;
                dead ->
                    Child ! stop,
                    receive
                        {'DOWN', Fence, process, Child, normal} -> Child
                    end;
                named_self ->
                    register(lynx_monitor_target, self()),
                    lynx_monitor_target;
                named_live ->
                    register(lynx_monitor_target, Child),
                    lynx_monitor_target;
                missing ->
                    lynx_monitor_missing
            end,
        Object =
            case is_atom(Target) of
                true -> {Target, node()};
                false -> Target
            end,
        ExpectDown =
            State =:= dead orelse State =:= missing orelse
                (Type =:= port andalso is_atom(Target)),
        Result =
            try
                Ref =
                    case Options of
                        two -> monitor(Type, Target);
                        _ -> monitor(Type, Target, Options)
                    end,
                Down =
                    case ExpectDown of
                        true ->
                            receive
                                {'DOWN', Ref, Type, ActualObject, Reason} ->
                                    {down, ActualObject =:= Object, Reason}
                            after 1000 -> missing_down
                            end;
                        false ->
                            none
                    end,
                {reference, Down, demonitor(Ref, [info]), erlang:unalias(Ref)}
            catch
                C:R -> {C, R}
            end,
        case State of
            dead ->
                ok;
            _ ->
                Child ! stop,
                receive
                    {'DOWN', Fence, process, Child, normal} -> ok
                end
        end,
        %% Keep identities out of the result; any unexpected queued signal is a failure.
        {Result, length(drain())}
    end).

self_link(Trap, Linked, Count) ->
    isolated(fun() ->
        process_flag(trap_exit, Trap),
        {Child, Ref} = spawn_monitor(fun() ->
            receive
                stop -> ok
            end
        end),
        case Linked of
            true -> link(Child);
            false -> ok
        end,
        {links, Before} = process_info(self(), links),
        self() ! retained,
        Observations = [
            begin
                First = link(self()),
                {links, AfterFirst} = process_info(self(), links),
                Second = link(self()),
                garbage_collect(),
                {links, AfterSecond} = process_info(self(), links),
                Unlink = unlink(self()),
                {links, AfterUnlink} = process_info(self(), links),
                {First, AfterFirst =:= Before, Second, AfterSecond =:= Before, Unlink,
                    AfterUnlink =:= Before}
            end
         || _ <- lists:seq(1, Count)
        ],
        Child ! stop,
        receive
            {'DOWN', Ref, process, Child, normal} -> ok
        end,
        Exit =
            case Trap andalso Linked of
                true ->
                    receive
                        {'EXIT', Child, normal} -> trapped
                    end;
                false ->
                    ignored
            end,
        %% A DOWN need not arrive after a linked-exit signal; settle the child link explicitly.
        true = unlink(Child),
        {links, Final} = process_info(self(), links),
        {Observations, Final =:= [], Exit, drain()}
    end).

relink(Owner, Trap, Final, Reason, Count) ->
    isolated(fun() ->
        process_flag(trap_exit, Trap),
        Parent = self(),
        {Child, Ref} = spawn_monitor(fun Loop() ->
            receive
                {relink, N} ->
                    Parent ! {self(), relink_steps(Parent, N)},
                    Loop();
                check ->
                    {links, Links} = process_info(self(), links),
                    Parent ! {self(), lists:member(Parent, Links)},
                    Loop();
                {stop, R} ->
                    exit(R)
            end
        end),
        true = link(Child),
        self() ! retained,
        Observations =
            case Owner of
                parent ->
                    relink_steps(Child, Count);
                child ->
                    Child ! {relink, Count},
                    receive
                        {Child, Steps} -> Steps
                    end
            end,
        %% The reply is behind link/unlink signals and old acknowledgements from this sender.
        Child ! check,
        PeerLinked =
            receive
                {Child, Linked} -> Linked
            end,
        {links, Links} = process_info(self(), links),
        LocalLinked = lists:member(Child, Links),
        case Final of
            linked -> ok;
            unlinked -> unlink(Child)
        end,
        Child ! check,
        PeerFinal =
            receive
                {Child, LastLinked} -> LastLinked
            end,
        {links, FinalLinks} = process_info(self(), links),
        LocalFinal = lists:member(Child, FinalLinks),
        Child ! {stop, Reason},
        Down =
            receive
                {'DOWN', Ref, process, Child, R} -> R
            end,
        Exit =
            case Trap andalso Final =:= linked of
                true ->
                    receive
                        {'EXIT', Child, R2} -> R2
                    after 1000 -> missing_exit
                    end;
                false ->
                    ignored
            end,
        {Observations, LocalLinked, PeerLinked, LocalFinal, PeerFinal, Down, Exit, drain()}
    end).

relink_steps(Pid, Count) ->
    [
        begin
            true = link(Pid),
            true = link(Pid),
            {links, Before} = process_info(self(), links),
            true = unlink(Pid),
            {links, Unlinked} = process_info(self(), links),
            true = link(Pid),
            {links, After} = process_info(self(), links),
            {lists:member(Pid, Before), lists:member(Pid, Unlinked), lists:member(Pid, After)}
        end
     || _ <- lists:seq(1, Count)
    ].

crossed_unlink(A, B, Final, Count) ->
    isolated(fun() -> [crossed_unlink_once(A, B, Final) || _ <- lists:seq(1, Count)] end).

crossed_unlink_once(A, B, Final) ->
    Parent = self(),
    {Child, Ref} = spawn_monitor(fun Loop() ->
        receive
            {go, Relink} ->
                crossed_unlink_action(Parent, Relink),
                Loop();
            link ->
                true = link(Parent),
                Loop();
            check ->
                {links, Links} = process_info(self(), links),
                Parent ! {self(), lists:member(Parent, Links)},
                Loop();
            stop ->
                ok
        end
    end),
    true = link(Child),
    true = crossed_unlink_check(Child),
    Child ! {go, B},
    crossed_unlink_action(Child, A),
    %% First fence peer operations, then our acknowledgements of those operations.
    _ = crossed_unlink_check(Child),
    Peer = crossed_unlink_check(Child),
    {links, Links} = process_info(self(), links),
    Local = lists:member(Child, Links),
    Consistent = Local =:= Peer andalso (A =/= B orelse Local =:= A),
    case Final of
        keep -> ok;
        local -> true = link(Child);
        peer -> Child ! link
    end,
    _ = crossed_unlink_check(Child),
    FinalPeer = crossed_unlink_check(Child),
    {links, FinalLinks} = process_info(self(), links),
    FinalLocal = lists:member(Child, FinalLinks),
    Valid =
        Consistent andalso FinalLocal =:= FinalPeer andalso
            (Final =:= keep orelse FinalLocal),
    true = unlink(Child),
    Child ! stop,
    receive
        {'DOWN', Ref, process, Child, normal} -> ok
    end,
    Valid.

crossed_unlink_check(Child) ->
    Child ! check,
    receive
        {Child, Linked} -> Linked
    end.

crossed_unlink_action(Pid, Relink) ->
    true = unlink(Pid),
    case Relink of
        true -> true = link(Pid);
        false -> ok
    end.

%% Compare an allowed-outcome predicate, not which participant wins the race.
unlink_exit_race(Reason, Count) ->
    [unlink_exit_race_once(Reason) || _ <- lists:seq(1, Count)].

unlink_exit_race_once(Reason) ->
    Parent = self(),
    {Child, ChildRef} = spawn_monitor(fun() ->
        receive
            go -> exit(Reason)
        end
    end),
    {Worker, WorkerRef} = spawn_monitor(fun() ->
        true = link(Child),
        Parent ! {self(), ready},
        unlink_wait_dead(Child),
        true = unlink(Child),
        Parent ! {self(), unlinked},
        receive
            stop -> ok
        end
    end),
    receive
        {Worker, ready} -> ok
    end,
    Child ! go,
    receive
        {'DOWN', ChildRef, process, Child, Reason} -> ok
    end,
    Worker ! stop,
    Down =
        receive
            {'DOWN', WorkerRef, process, Worker, R} -> R
        end,
    receive
        {Worker, unlinked} -> Down =:= normal
    after 0 -> Reason =/= normal andalso Down =:= Reason
    end.

unlink_wait_dead(Child) ->
    case is_process_alive(Child) of
        true -> unlink_wait_dead(Child);
        false -> ok
    end.

linked_exit(Mode, Reason) ->
    isolated(fun() ->
        process_flag(trap_exit, Mode =:= trapped),
        Child = spawn_link(fun() ->
            receive
                go -> exit(Reason)
            end
        end),
        Ref = monitor(process, Child),
        case Mode of
            unlinked -> unlink(Child);
            _ -> ok
        end,
        Child ! go,
        Exit =
            case Mode of
                trapped ->
                    receive
                        {'EXIT', Child, R} -> R
                    end;
                _ ->
                    ignored
            end,
        receive
            {'DOWN', Ref, process, Child, Down} -> {Exit, Down}
        end
    end).

exit_signal(Trap, Reason) ->
    {_, _, Down, Observation, _} = exit_signal_run(Trap, Reason, unlinked),
    {Down, Observation}.

exit_signal(Trap, Reason, Mode) ->
    isolated(fun() ->
        process_flag(trap_exit, true),
        exit_signal_run(Trap, Reason, Mode)
    end).

exit_signal_run(Trap, Reason, Mode) ->
    Parent = self(),
    {Child, Ref} = spawn_monitor(fun() ->
        process_flag(trap_exit, Trap),
        Parent ! {self(), ready},
        case Mode of
            unlink_unfenced ->
                ok;
            _ ->
                receive
                    check_link ->
                        {links, Links} = process_info(self(), links),
                        Parent ! {self(), {peer_link, lists:member(Parent, Links)}}
                end
        end,
        receive
            {'EXIT', Parent, R} -> Parent ! {self(), {trapped, R}};
            ping -> Parent ! {self(), ignored}
        end
    end),
    receive
        {Child, ready} -> ok
    end,
    case Mode of
        unlinked ->
            ok;
        linked ->
            link(Child);
        _ ->
            link(Child),
            unlink(Child)
    end,
    PeerLinked =
        case Mode of
            unlink_unfenced ->
                unobserved;
            _ ->
                Child ! check_link,
                receive
                    {Child, {peer_link, Linked}} -> Linked
                end
        end,
    {links, LocalLinks} = process_info(self(), links),
    LocalLinked = lists:member(Child, LocalLinks),
    true = exit(Child, Reason),
    Child ! ping,
    Down =
        receive
            {'DOWN', Ref, process, Child, R1} -> R1
        end,
    Exit =
        case Mode of
            linked ->
                receive
                    {'EXIT', Child, R2} -> R2
                after 1000 -> missing_exit
                end;
            _ ->
                unlinked
        end,
    Observation =
        receive
            {Child, Value} -> Value
        after 0 -> no_reply
        end,
    {LocalLinked, PeerLinked, Down, Observation, Exit}.

self_exit(Arity, Trap, Linked, Reason) ->
    isolated(fun() ->
        process_flag(trap_exit, true),
        Parent = self(),
        Options =
            case Linked of
                true -> [link, monitor];
                false -> [monitor]
            end,
        {Child, Ref} = spawn_opt(
            fun() ->
                process_flag(trap_exit, Trap),
                Self = self(),
                Result =
                    try
                        case Arity of
                            one -> exit(Reason);
                            two -> exit(Self, Reason)
                        end
                    catch
                        C:R -> {caught, C, R}
                    after
                        Parent ! {self(), cleanup}
                    end,
                Message =
                    receive
                        {'EXIT', Self, R0} -> {trapped, R0}
                    after 0 -> none
                    end,
                Parent ! {self(), {returned, Result, Message}}
            end,
            [{min_heap_size, 100} | Options]
        ),
        Down =
            receive
                {'DOWN', Ref, process, Child, R1} -> R1
            end,
        Exit =
            case Linked of
                true ->
                    receive
                        {'EXIT', Child, R2} -> R2
                    after 1000 -> missing_exit
                    end;
                false ->
                    unlinked
            end,
        Observations = [
            case M of
                {Child, Value} -> Value;
                _ -> unexpected
            end
         || M <- drain()
        ],
        {Down, Exit, Observations}
    end).

trap_transition(queued_self, Final, Reason) ->
    isolated(fun() ->
        false = process_flag(trap_exit, true),
        Self = self(),
        true = exit(Self, Reason),
        %% Fence reception while trapping is on, leaving the real EXIT queued.
        Self ! sent,
        receive
            sent -> ok
        end,
        true = process_flag(trap_exit, Final),
        receive
            {'EXIT', Self, R} -> {true, {trapped, R}}
        after 0 -> missing_message
        end
    end);
trap_transition(Mode, Final, Reason) ->
    isolated(fun() ->
        Initial =
            case Mode of
                queued_explicit -> true;
                _ -> not Final
            end,
        false = process_flag(trap_exit, Initial),
        Parent = self(),
        Links =
            case Mode of
                before_linked -> [link];
                _ -> []
            end,
        {Child, Ref} = spawn_opt(
            fun() ->
                receive
                    go ->
                        case Mode of
                            before_linked ->
                                exit(Reason);
                            _ ->
                                true = exit(Parent, Reason),
                                Parent ! {self(), sent}
                        end
                end
            end,
            [monitor, {min_heap_size, 100} | Links]
        ),
        case Mode of
            queued_explicit ->
                Child ! go,
                %% This sender's earlier exit is processed while trapping is on.
                %% Selectively receiving its marker leaves the real EXIT queued.
                receive
                    {Child, sent} -> ok
                end;
            _ ->
                ok
        end,
        Previous = process_flag(trap_exit, Final),
        Initial = Previous,
        case Mode of
            queued_explicit -> ok;
            _ -> Child ! go
        end,
        Down =
            receive
                {'DOWN', Ref, process, Child, R1} -> R1
            end,
        Wait =
            case {Mode, Final, Reason} of
                {M, false, normal} when M =/= queued_explicit -> 0;
                _ -> 1000
            end,
        Message =
            receive
                {'EXIT', Child, R2} -> {trapped, R2}
            after Wait ->
                case Wait of
                    0 -> none;
                    _ -> missing_exit
                end
            end,
        {Previous, Down, Message}
    end).

monitor_options(unknown, Options) ->
    {demonitor(make_ref(), Options), []};
monitor_options(State, Options) ->
    Child = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Ref = monitor(process, Child),
    case State of
        active ->
            ok;
        active_message ->
            self() ! {other, Ref, a, b, c};
        _ ->
            Child ! stop,
            receive
                {'DOWN', Ref, process, Child, normal} = Down ->
                    %% Requeue a real DOWN so its presence is not a scheduling race.
                    case State of
                        queued ->
                            self() ! Down;
                        queued_twice ->
                            self() ! Down,
                            self() ! Down;
                        queued_other ->
                            self() ! {other, Ref, a, b, c},
                            self() ! Down;
                        consumed ->
                            ok
                    end
            end
    end,
    Result =
        try
            demonitor(Ref, Options)
        catch
            C:R -> {C, R}
        end,
    case State of
        S when S =:= active; S =:= active_message ->
            Fence = monitor(process, Child),
            Child ! stop,
            receive
                {'DOWN', Fence, process, Child, normal} -> ok
            end;
        _ ->
            ok
    end,
    Remaining = [
        case M of
            {'DOWN', Ref, process, Child, R0} -> {down, R0};
            {other, Ref, a, b, c} -> other;
            _ -> unexpected
        end
     || M <- drain()
    ],
    {Result, Remaining}.

%% A name lookup binds the monitor to that process, not to future owners of the name.
monitor_rebind(Mode, Action, Order) ->
    isolated(fun() ->
        Options =
            case Mode of
                plain -> [];
                _ -> [{alias, Mode}]
            end,
        {Original, OriginalFence} = spawn_monitor(fun() ->
            receive
                stop -> exit(original)
            end
        end),
        {Replacement, ReplacementFence} = spawn_monitor(fun() ->
            receive
                stop -> exit(replacement)
            end
        end),
        true = register(lynx_rebound_monitor, Original),
        OriginalRef = monitor(process, lynx_rebound_monitor, Options),
        true = unregister(lynx_rebound_monitor),
        true = register(lynx_rebound_monitor, Replacement),
        ReplacementRef = monitor(process, lynx_rebound_monitor, Options),
        Rebound = whereis(lynx_rebound_monitor) =:= Replacement,
        Acted =
            case Action of
                keep ->
                    kept;
                demonitor ->
                    demonitor(OriginalRef, [info]);
                unalias ->
                    erlang:unalias(OriginalRef);
                reply ->
                    OriginalRef ! reply,
                    receive
                        reply -> replied
                    end
            end,
        Cancelled = Action =:= demonitor orelse (Action =:= reply andalso Mode =:= reply_demonitor),
        Cancellation =
            case Cancelled of
                true -> demonitor(OriginalRef, [info]);
                false -> not_cancelled
            end,
        Targets =
            case Order of
                original_first ->
                    [
                        {Original, OriginalFence, original},
                        {Replacement, ReplacementFence, replacement}
                    ];
                replacement_first ->
                    [
                        {Replacement, ReplacementFence, replacement},
                        {Original, OriginalFence, original}
                    ]
            end,
        Registrations = [
            begin
                Pid ! stop,
                receive
                    {'DOWN', Fence, process, Pid, Reason} -> ok
                end,
                whereis(lynx_rebound_monitor) =:= Replacement
            end
         || {Pid, Fence, Reason} <- Targets
        ],
        Object = {lynx_rebound_monitor, node()},
        OriginalDown =
            case Cancelled of
                true ->
                    cancelled;
                false ->
                    receive
                        {'DOWN', OriginalRef, process, OldObject, OldReason} ->
                            {OldObject =:= Object, OldReason}
                    after 1000 -> missing_original_down
                    end
            end,
        ReplacementDown =
            receive
                {'DOWN', ReplacementRef, process, NewObject, NewReason} ->
                    {NewObject =:= Object, NewReason}
            after 1000 -> missing_replacement_down
            end,
        {Rebound, Acted, Cancellation, Registrations, OriginalDown, ReplacementDown,
            erlang:unalias(OriginalRef), erlang:unalias(ReplacementRef),
            whereis(lynx_rebound_monitor), length(drain())}
    end).

alias_lifetime(standalone, Options) ->
    Alias =
        case Options of
            default -> erlang:alias();
            _ -> erlang:alias(Options)
        end,
    Alias ! first,
    Alias ! second,
    receive
        first -> ok
    end,
    Removed = erlang:unalias(Alias),
    Alias ! dropped,
    {Removed, erlang:unalias(Alias), drain()};
alias_lifetime(Mode, Event) ->
    Child = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Ref = monitor(process, Child, [{alias, Mode}]),
    End =
        case Event of
            demonitor ->
                Ref ! before_end,
                receive
                    before_end -> ok
                end,
                Result = demonitor(Ref, [info]),
                Fence = monitor(process, Child),
                Child ! stop,
                receive
                    {'DOWN', Fence, process, Child, normal} -> ok
                end,
                Result;
            down ->
                Child ! stop,
                receive
                    {'DOWN', Ref, process, Child, normal} -> down
                end;
            unalias ->
                Result = erlang:unalias(Ref),
                Child ! stop,
                receive
                    {'DOWN', Ref, process, Child, normal} -> ok
                end,
                Result
        end,
    Ref ! after_end,
    %% Establish delivery before removing an explicit alias that survived the event.
    self() ! alias_fence,
    receive
        alias_fence -> ok
    end,
    Removed = erlang:unalias(Ref),
    {End, Removed, drain()}.

alias_owner(Mode, Action) ->
    isolated(fun() ->
        Parent = self(),
        {Target, Fence} = spawn_monitor(fun() ->
            receive
                stop -> ok
            end
        end),
        Ref =
            case Mode of
                {standalone, Option} -> erlang:alias([Option]);
                {monitor, plain} -> monitor(process, Target);
                {monitor, Option} -> monitor(process, Target, [{alias, Option}])
            end,
        self() ! {'DOWN', Ref, process, Target, owner_sentinel},
        self() ! {keep, Ref},
        {Other, OtherFence} = spawn_opt(
            fun() ->
                OwnRef = monitor(process, Target),
                self() ! {other, Ref, process, Target, foreign_sentinel},
                self() ! {'DOWN', OwnRef, process, Target, own_sentinel},
                self() ! {keep, Ref},
                Options =
                    case Action of
                        {demonitor, Opts} -> Opts;
                        _ -> []
                    end,
                Result =
                    try
                        case Action of
                            unalias -> erlang:unalias(Ref);
                            demonitor -> demonitor(Ref);
                            {demonitor, Options} -> demonitor(Ref, Options)
                        end
                    catch
                        C:R -> {C, R}
                    end,
                %% OTP permits badarg when foreign-monitor ownership is cheap
                %% to check. Otherwise the reference behaves as an absent monitor.
                Allowed =
                    case Action of
                        unalias ->
                            Result =:= false;
                        _ ->
                            Result =:= not lists:member(info, Options) orelse
                                (element(1, Mode) =:= monitor andalso Result =:= {error, badarg})
                    end,
                Flushed = Result =/= {error, badarg} andalso lists:member(flush, Options),
                Remaining = lists:sort([
                    case M of
                        {other, Ref, process, Target, foreign_sentinel} -> foreign_down;
                        {'DOWN', OwnRef, process, Target, own_sentinel} -> own_down;
                        {keep, Ref} -> keep;
                        _ -> unexpected
                    end
                 || M <- drain()
                ]),
                Expected =
                    case Flushed of
                        true -> [keep, own_down];
                        false -> [foreign_down, keep, own_down]
                    end,
                case Mode of
                    {monitor, plain} ->
                        ok;
                    _ ->
                        Ref ! {via_alias, first},
                        Ref ! {via_alias, second}
                end,
                Parent ! {self(), {attempt, Allowed, Remaining =:= Expected}},
                receive
                    collect -> ok
                end,
                receive
                    {'DOWN', OwnRef, process, Target, Down} ->
                        Parent ! {self(), {own_down, Down}}
                end
            end,
            [monitor, {min_heap_size, 100}]
        ),
        {Allowed, CallerMailbox} =
            receive
                {Other, {attempt, A, B}} -> {A, B}
            end,
        Mailbox = lists:sort([
            case M of
                {'DOWN', Ref, process, Target, owner_sentinel} -> owner_down;
                {keep, Ref} -> keep;
                {via_alias, Value} -> Value;
                _ -> unexpected
            end
         || M <- drain()
        ]),
        Removed = erlang:unalias(Ref),
        ActiveMonitor = demonitor(Ref, [info]),
        case Mode of
            {monitor, plain} -> ok;
            _ -> Ref ! dropped
        end,
        self() ! fence,
        receive
            fence -> ok
        end,
        Late = drain(),
        Target ! stop,
        receive
            {'DOWN', Fence, process, Target, normal} -> ok
        end,
        Other ! collect,
        OwnDown =
            receive
                {Other, {own_down, R1}} -> R1
            end,
        receive
            {'DOWN', OtherFence, process, Other, normal} -> ok
        end,
        {Allowed, CallerMailbox, Mailbox, Removed, ActiveMonitor, Late, OwnDown}
    end).

target_cancel(Mode, State, Action) ->
    isolated(fun() ->
        Parent = self(),
        {Target, Fence} = spawn_opt(
            fun() ->
                receive
                    {run, Ref} ->
                        Other = make_ref(),
                        self() ! {other, Ref, a, b, c},
                        self() ! {other, Other, a, b, c},
                        self() ! keep,
                        Result =
                            try
                                case Action of
                                    unalias -> erlang:unalias(Ref);
                                    demonitor -> demonitor(Ref);
                                    {demonitor, Options} -> demonitor(Ref, Options)
                                end
                            catch
                                C:R -> {C, R}
                            end,
                        Remaining = lists:sort([
                            case M of
                                {other, Ref, a, b, c} -> matching;
                                {other, Other, a, b, c} -> unrelated;
                                keep -> keep;
                                _ -> unexpected
                            end
                         || M <- drain()
                        ]),
                        case Mode of
                            plain ->
                                ok;
                            _ ->
                                Ref ! {via_alias, first},
                                Ref ! {via_alias, second}
                        end,
                        Parent ! {self(), Result, Remaining},
                        receive
                            stop -> ok
                        end
                end
            end,
            [monitor, {min_heap_size, 100}]
        ),
        Ref =
            case Mode of
                plain -> monitor(process, Target);
                _ -> monitor(process, Target, [{alias, Mode}])
            end,
        self() ! {'DOWN', Ref, process, Target, owner_sentinel},
        self() ! keep,
        case State of
            active -> ok;
            removed -> true = demonitor(Ref, [info])
        end,
        %% The monitor install/removal precedes this message from the same owner.
        Target ! {run, Ref},
        {Result, CallerMailbox} =
            receive
                {Target, Return, Messages} -> {Return, Messages}
            end,
        Mailbox = lists:sort([
            case M of
                {'DOWN', Ref, process, Target, owner_sentinel} -> owner_down;
                {via_alias, Value} -> Value;
                keep -> keep;
                _ -> unexpected
            end
         || M <- drain()
        ]),
        Removed = erlang:unalias(Ref),
        ActiveMonitor = demonitor(Ref, [info]),
        Target ! stop,
        receive
            {'DOWN', Fence, process, Target, normal} -> ok
        end,
        {Result, CallerMailbox, Mailbox, Removed, ActiveMonitor}
    end).

monitor_pair(FirstMode, SecondMode, Action) ->
    isolated(fun() ->
        {Target, Fence} = spawn_opt(
            fun() ->
                receive
                    stop -> ok
                end
            end,
            [monitor, {min_heap_size, 100}]
        ),
        Refs = [
            {Slot, Mode,
                case Mode of
                    plain -> monitor(process, Target);
                    _ -> monitor(process, Target, [{alias, Mode}])
                end}
         || {Slot, Mode} <- [{first, FirstMode}, {second, SecondMode}]
        ],
        [{first, _, FirstRef}, _] = Refs,
        Result =
            case Action of
                keep -> kept;
                demonitor -> demonitor(FirstRef, [info]);
                unalias -> erlang:unalias(FirstRef);
                reply -> FirstRef ! {reply, first}
            end,
        %% Fence the reply before terminating the target, not against its DOWN.
        self() ! pair_fence,
        receive
            pair_fence -> ok
        end,
        Target ! stop,
        Downs = [
            case
                Slot =:= first andalso
                    (Action =:= demonitor orelse
                        (Action =:= reply andalso Mode =:= reply_demonitor))
            of
                true ->
                    cancelled;
                false ->
                    receive
                        {'DOWN', Ref, process, Target, Reason} -> Reason
                    end
            end
         || {Slot, Mode, Ref} <- Refs
        ],
        receive
            {'DOWN', Fence, process, Target, normal} -> ok
        end,
        [
            begin
                Ref ! {late, Slot, one},
                Ref ! {late, Slot, two}
            end
         || {Slot, Mode, Ref} <- Refs, Mode =/= plain
        ],
        self() ! pair_fence,
        receive
            pair_fence -> ok
        end,
        Messages = lists:sort([
            case M of
                {reply, first} -> M;
                {late, _, _} -> M;
                _ -> unexpected
            end
         || M <- drain()
        ]),
        Removed = [erlang:unalias(Ref) || {_, _, Ref} <- Refs],
        Active = [demonitor(Ref, [info]) || {_, _, Ref} <- Refs],
        {Result, Downs, Messages, Removed, Active}
    end).

termination_queue(Count) ->
    Child = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Refs = [{N, monitor(process, Child)} || N <- lists:seq(1, Count)],
    [demonitor(Ref, [flush]) || {N, Ref} <- Refs, N rem 2 =:= 0],
    [Child ! {unmatched, N, <<N:32>>} || N <- lists:seq(1, Count)],
    true = exit(Child, kill),
    Reasons = [
        receive
            {'DOWN', Ref, process, Child, R} -> R
        end
     || {N, Ref} <- Refs, N rem 2 =:= 1
    ],
    {Reasons, [demonitor(Ref, [info]) || {_, Ref} <- Refs], drain()}.

termination_cascade(Count, Order) ->
    Parent = self(),
    {Target, TargetRef} = spawn_monitor(fun() ->
        receive
            stop -> ok
        end
    end),
    {Observer, ObserverRef} = spawn_monitor(fun() ->
        [monitor(process, Target) || _ <- lists:seq(1, Count)],
        Parent ! {self(), ready},
        receive
            stop -> ok
        end
    end),
    receive
        {Observer, ready} -> ok
    end,
    case Order of
        target_first ->
            exit(Target, kill),
            exit(Observer, kill);
        observer_first ->
            exit(Observer, kill),
            exit(Target, kill)
    end,
    TargetReason =
        receive
            {'DOWN', TargetRef, process, Target, R1} -> R1
        end,
    ObserverReason =
        receive
            {'DOWN', ObserverRef, process, Observer, R2} -> R2
        end,
    {TargetReason, ObserverReason, drain()}.

%% No sender can satisfy the first positive timeout: the protected computation
%% must traverse a timer wait before reaching its message/timeout continuation.
suspended_receive(Mode) ->
    isolated(fun() ->
        erase(),
        self() ! retained,
        Value =
            try
                receive
                    never -> unexpected
                after 1 -> ok
                end,
                put(waited, true),
                case Mode of
                    timeout ->
                        receive
                            never -> unexpected
                        after 1 -> throw(timeout_body)
                        end;
                    message ->
                        Me = self(),
                        spawn(fun() -> Me ! resume end),
                        receive
                            resume -> throw(message_body)
                        end;
                    nested ->
                        try
                            receive
                                never -> unexpected
                            after 1 -> error(inner)
                            end
                        catch
                            error:inner -> throw(outer)
                        end
                end
            catch
                C:R -> {C, R}
            after
                put(cleanup, ran)
            end,
        {Value, get(waited), get(cleanup), drain()}
    end).

missing_link(Trap) ->
    isolated(fun() ->
        process_flag(trap_exit, Trap),
        {Child, Ref} = spawn_monitor(fun() -> ok end),
        receive
            {'DOWN', Ref, process, Child, normal} -> ok
        end,
        try link(Child) of
            Linked ->
                receive
                    {'EXIT', Child, Reason} -> {Linked, Reason, drain()}
                end
        catch
            C:R -> {exception, C, R}
        end
    end).

process_liveness() ->
    isolated(fun() ->
        {Child, Ref} = spawn_monitor(fun() ->
            receive
                stop -> ok
            end
        end),
        Alive = {is_process_alive(self()), is_process_alive(Child)},
        Unnamed = process_info(Child, registered_name),
        true = register(lynx_liveness_child, Child),
        Named = {whereis(lynx_liveness_child) =:= Child, process_info(Child, registered_name)},
        true = unregister(lynx_liveness_child),
        Unregistered = {whereis(lynx_liveness_child), process_info(Child, registered_name)},
        Child ! stop,
        receive
            {'DOWN', Ref, process, Child, normal} -> ok
        end,
        Dead = {is_process_alive(Child), process_info(Child, registered_name)},
        {Alive, Unnamed, Named, Unregistered, Dead}
    end).

raise_exception(Class, Reason, Stack) ->
    try erlang:raise(Class, Reason, Stack) of
        Value -> {returned, Value}
    catch
        C:R:S -> {raised, C, R, S =:= Stack}
    end.

trace_depth(default, Count, Style) ->
    trace_depth_result(Count, Style);
trace_depth(Depth, Count, Style) ->
    Previous = erlang:system_flag(backtrace_depth, Depth),
    try
        Stored = erlang:system_flag(backtrace_depth, Depth),
        {Stored, trace_depth_result(Count, Style)}
    after
        erlang:system_flag(backtrace_depth, Previous)
    end.

trace_depth_result(Count, generated) ->
    try
        ?MODULE:trace_depth_a(Count)
    catch
        error:depth_error:Stack -> {generated, length(Stack)}
    end;
trace_depth_result(Count, {child, Style}) ->
    isolated(fun() -> trace_depth_result(Count, Style) end);
trace_depth_result(Count, {Style, Class}) ->
    Modern = [{?MODULE, supplied, I, []} || I <- lists:seq(1, Count)],
    Stack =
        case Style of
            modern -> Modern;
            legacy -> [{?MODULE, supplied, I} || I <- lists:seq(1, Count)];
            invalid_suffix -> Modern ++ [bad];
            improper -> Modern ++ bad
        end,
    try apply(erlang, raise, [Class, supplied, Stack]) of
        Value -> {returned, Value}
    catch
        C:R:S -> {raised, C, R, S}
    end.

%% Remote calls prevent the compiler proving this recursion never returns
%% and deleting its post-call arithmetic (which would make it tail-recursive).
trace_depth_a(0) -> error(depth_error);
trace_depth_a(N) -> ?MODULE:trace_depth_b(N - 1) + 1.
trace_depth_b(N) -> ?MODULE:trace_depth_a(N - 1) + 2.

%% Never serialize function identities or compiler-generated closure names.
function_value(local_zero, Captured) -> fun() -> {Captured, zero} end;
function_value(local_one, Captured) -> fun(X) -> {Captured, X} end;
function_value(local_two, Captured) -> fun(X, Y) -> {Captured, X, Y} end;
function_value(external_zero, _) -> fun ?MODULE:function_zero/0;
function_value(external_one, _) -> fun ?MODULE:function_one/1;
function_value(external_two, _) -> fun ?MODULE:function_two/2;
function_value(bif_zero, _) -> fun erlang:self/0;
function_value(bif_one, _) -> fun erlang:abs/1;
function_value(missing_export, _) -> fun ?MODULE:missing/0;
function_value(missing_module, _) -> fun lynx_missing_functions:missing/0;
function_value(not_fun, Captured) -> Captured.

function_zero() -> zero.
function_one(X) -> {one, X}.
function_two(X, Y) -> {two, X, Y}.

function_apply(Kind, Captured, Args) ->
    Fun = function_value(Kind, Captured),
    try apply(Fun, Args) of
        Pid when is_pid(Pid) -> {ok, {pid, Pid =:= self()}};
        Value -> {ok, Value}
    catch
        error:{badarity, {BadFun, BadArgs}} ->
            {error, badarity, BadFun =:= Fun, BadArgs =:= Args};
        error:{badfun, BadFun} ->
            {error, badfun, BadFun =:= Fun};
        C:R ->
            {C, R}
    end.

function_named(Module, Name, Args) ->
    try apply(Module, Name, Args) of
        Value -> {ok, Value}
    catch
        C:R -> {C, R}
    end.

function_make(Module, Name, Arity, Args) ->
    Fun = erlang:make_fun(Module, Name, Arity),
    {
        is_function(Fun),
        erlang:fun_info(Fun, module),
        erlang:fun_info(Fun, name),
        erlang:fun_info(Fun, arity),
        function_apply(not_fun, Fun, Args)
    }.

function_spawn(Kind, Mode, Value) ->
    isolated(fun() ->
        process_flag(trap_exit, true),
        Parent = self(),
        Fun =
            case Kind of
                local -> fun() -> Parent ! {self(), Value} end;
                external -> fun ?MODULE:function_child/0;
                external_error -> fun ?MODULE:function_child_error/0;
                external_throw -> fun ?MODULE:function_child_throw/0;
                external_exit -> fun ?MODULE:function_child_exit/0;
                _ -> function_value(Kind, Value)
            end,
        Options =
            case Mode of
                monitored -> [monitor];
                linked -> [link, monitor];
                small_heap -> [monitor, {min_heap_size, 100}]
            end,
        try spawn_opt(Fun, Options) of
            {Child, Ref} ->
                Child ! {Parent, Value},
                receive
                    {'DOWN', Ref, process, Child, Reason} ->
                        Messages = [
                            case M of
                                {Child, V} -> {reply, V};
                                {'EXIT', Child, R} -> {exit, function_exit(R, Fun)};
                                _ -> unexpected
                            end
                         || M <- drain()
                        ],
                        {child, function_exit(Reason, Fun), Messages}
                end
        catch
            C:R -> {caller, C, R}
        end
    end).

function_exit({{badarity, {Fun, Args}}, Stack}, Expected) ->
    {badarity, Fun =:= Expected, Args =:= [], is_list(Stack)};
function_exit({Reason, Stack}, _) when is_list(Stack) -> {Reason, stack};
function_exit(Reason, _) ->
    Reason.

function_child() ->
    receive
        {Parent, Value} -> Parent ! {self(), Value}
    end.

function_child_error() ->
    function_child(),
    error(child_error).

function_child_throw() ->
    function_child(),
    throw(child_throw).

function_child_exit() ->
    function_child(),
    exit(child_exit).

utf(Encoding, Bits, Offset) ->
    <<_:Offset, Input/bitstring>> = Bits,
    case Encoding of
        utf8 ->
            case Input of
                <<C/utf8, R/bitstring>> -> {C, R};
                _ -> no
            end;
        utf16 ->
            case Input of
                <<C/utf16, R/bitstring>> -> {C, R};
                _ -> no
            end;
        utf16le ->
            case Input of
                <<C/utf16-little, R/bitstring>> -> {C, R};
                _ -> no
            end;
        utf32 ->
            case Input of
                <<C/utf32, R/bitstring>> -> {C, R};
                _ -> no
            end;
        utf32le ->
            case Input of
                <<C/utf32-little, R/bitstring>> -> {C, R};
                _ -> no
            end
    end.

utf_construct(Encoding, C, Offset) ->
    case Encoding of
        utf8 -> <<5:Offset, C/utf8, 3:2>>;
        utf16 -> <<5:Offset, C/utf16, 3:2>>;
        utf16le -> <<5:Offset, C/utf16-little, 3:2>>;
        utf32 -> <<5:Offset, C/utf32, 3:2>>;
        utf32le -> <<5:Offset, C/utf32-little, 3:2>>
    end.

bits(Value, Width, Offset, big) ->
    Bits = <<0:Offset, Value:Width/signed-big, 5:3>>,
    <<_:Offset, Read:Width/signed-big, Tail/bitstring>> = Bits,
    {bit_size(Bits), Read, Tail, Bits};
bits(Value, Width, Offset, little) ->
    Bits = <<0:Offset, Value:Width/signed-little, 5:3>>,
    <<_:Offset, Read:Width/signed-little, Tail/bitstring>> = Bits,
    {bit_size(Bits), Read, Tail, Bits}.

%% Return raw encodings, including overflow to infinity, without decoding floats.
float_bits(Value, Width, Offset, big) ->
    Bits = <<5:Offset, Value:Width/float-big, 3:3>>,
    <<_:Offset, Encoded:Width/bitstring, _:3>> = Bits,
    {Bits, Encoded};
float_bits(Value, Width, Offset, little) ->
    Bits = <<5:Offset, Value:Width/float-little, 3:3>>,
    <<_:Offset, Encoded:Width/bitstring, _:3>> = Bits,
    {Bits, Encoded}.
