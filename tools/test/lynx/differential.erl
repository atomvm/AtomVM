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
    process_exchange/1,
    monitor_exit/1,
    linked_exit/2,
    exit_signal/2,
    monitor_options/2,
    alias_lifetime/2,
    termination_queue/1,
    termination_cascade/2,
    suspended_receive/1,
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
    Parent = self(),
    {Child, Ref} = spawn_monitor(fun() ->
        process_flag(trap_exit, Trap),
        Parent ! {self(), ready},
        receive
            {'EXIT', Parent, R} -> Parent ! {self(), {trapped, R}};
            ping -> Parent ! {self(), ignored}
        end
    end),
    receive
        {Child, ready} -> ok
    end,
    true = exit(Child, Reason),
    Child ! ping,
    receive
        {'DOWN', Ref, process, Child, Down} ->
            Observation =
                receive
                    {Child, Value} -> Value
                after 0 -> no_reply
                end,
            {Down, Observation}
    end.

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

alias_lifetime(standalone, Options) ->
    Alias = erlang:alias(Options),
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
    Removed = erlang:unalias(Ref),
    {End, Removed, drain()}.

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
