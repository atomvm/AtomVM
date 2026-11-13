% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(differential).
-export([
    compare/2,
    arithmetic/3,
    maps/3,
    map_sequence/2,
    dictionary_sequence/2,
    binary_sequence/2,
    bits/4,
    float_bits/4,
    call/3,
    execution_paths/3,
    print_result/1,
    map_entries/1,
    map_fold/2,
    map_callback/3,
    set_elements/1,
    set_algebra/2,
    dictionary/2,
    float_text/2,
    short_circuit/3,
    receive_timeout/2,
    receive_guards/3,
    receive_sequence/2,
    timer/3,
    timer_options/3,
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
    function_compare/2,
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
    utf_construct/3,
    map_relations/3,
    map_singleton_iteration/2,
    map_guards/2,
    list_relations/4,
    seq_contract/4
]).

%% Timer observations use delivery/monitor handshakes, not elapsed-time
%% comparisons. The outer runner bounds hangs; no short internal deadline
%% turns scheduler latency into a semantic mismatch.
timer(deliver, Delay, Payload) ->
    Token = make_ref(),
    self() ! retained,
    Ref = erlang:send_after(Delay, self(), {Token, Payload}),
    Received =
        receive
            {Token, Value} -> Value
        end,
    {is_reference(Ref), Received, drain()};
timer(cancel, Delay, Payload) ->
    Token = make_ref(),
    self() ! retained,
    Ref = erlang:send_after(Delay, self(), {Token, Payload}),
    Canceled = erlang:cancel_timer(Ref),
    Valid =
        case Canceled of
            false ->
                receive
                    {Token, Payload} -> true
                end;
            Remaining when is_integer(Remaining), Remaining >= 0, Remaining =< Delay -> true;
            _ ->
                false
        end,
    {is_reference(Ref), Valid, erlang:cancel_timer(Ref), drain()};
timer(expired, Delay, Payload) ->
    Token = make_ref(),
    Ref = erlang:send_after(Delay, self(), {Token, Payload}),
    Received =
        receive
            {Token, Value} -> Value
        end,
    {Received, erlang:cancel_timer(Ref), erlang:cancel_timer(Ref)};
timer(Mode, Delay, Payload) when Mode =:= dead_before; Mode =:= dead_after ->
    {Child, Monitor} = spawn_monitor(fun() ->
        receive
            stop -> ok
        end
    end),
    case Mode of
        dead_before ->
            Child ! stop,
            receive
                {'DOWN', Monitor, process, Child, normal} -> ok
            end,
            Ref = erlang:send_after(Delay, Child, Payload);
        dead_after ->
            Ref = erlang:send_after(Delay, Child, Payload),
            Child ! stop,
            receive
                {'DOWN', Monitor, process, Child, normal} -> ok
            end
    end,
    Canceled = erlang:cancel_timer(Ref),
    %% Remaining time varies with scheduling; keep false distinct from a
    %% successful cancellation and validate rather than compare milliseconds.
    Result =
        case Canceled of
            N when is_integer(N) -> {remaining, N >= 0 andalso N =< Delay};
            Other -> Other
        end,
    {is_reference(Ref), Result, erlang:cancel_timer(Ref), drain()}.

%% Absolute deadlines are constructed in each VM's own monotonic epoch.
%% Keep the not-before observation, but never serialize clock values or demand
%% an upper latency bound. A mistaken relative/absolute interpretation must not
%% pass just because the message eventually arrived.
timer_options(Mode, Delay, Options) ->
    Now = erlang:monotonic_time(millisecond),
    %% Do not subtract from a fresh VM's clock: that can precede its start_time
    %% and correctly raise badarg. Age an observed timestamp instead.
    Deadline = Now + max(0, Delay),
    case Delay < 0 of
        true -> receive
            after -Delay -> ok
            end;
        false -> ok
    end,
    Time =
        case Mode of
            abs_deliver -> Deadline;
            abs_cancel -> Deadline;
            _ -> Delay
        end,
    Token = make_ref(),
    Payload = {nested, #{1 => one, 1.0 => float}, <<5:3>>},
    self() ! retained,
    Ref = erlang:send_after(Time, self(), {Token, Payload}, Options),
    case Mode of
        deliver ->
            timer_options_delivered(Ref, Token, Deadline);
        abs_deliver ->
            timer_options_delivered(Ref, Token, Deadline);
        _ ->
            Valid =
                case erlang:cancel_timer(Ref) of
                    false ->
                        receive
                            {Token, Payload} -> erlang:monotonic_time(millisecond) >= Deadline
                        end;
                    N when is_integer(N), N >= 0 -> N =< max(0, Delay);
                    _ ->
                        false
                end,
            {is_reference(Ref), Valid, erlang:cancel_timer(Ref), drain()}
    end.

timer_options_delivered(Ref, Token, Deadline) ->
    receive
        {Token, Payload} ->
            NotEarly = erlang:monotonic_time(millisecond) >= Deadline,
            {
                is_reference(Ref),
                Payload,
                NotEarly,
                erlang:cancel_timer(Ref),
                erlang:cancel_timer(Ref),
                drain()
            }
    end.

%% Inputs cross a module boundary to prevent constant-folded operations.
call(Module, Function, Args) -> apply(Module, Function, Args).

%% Compare observations, not equality between paths: guards reject errors and
%% map patterns fail to match where body/dynamic lookups raise exceptions.
execution_paths(Op, A, B) ->
    Pattern =
        case Op of
            map_get -> [{pattern, path_map_pattern(A, B)}];
            _ -> []
        end,
    [
        {body, path_result(fun() -> path_body(Op, A, B) end)},
        {dynamic, path_result(fun() -> call(erlang, Op, [A, B]) end)},
        {guard, path_guard(Op, A, B)}
        | Pattern
    ].

path_result(Fun) ->
    try Fun() of
        Value when is_float(Value) -> {ok, Value, <<Value/float>>};
        Value -> {ok, Value}
    catch
        Class:Reason -> {exception, Class, Reason}
    end.

path_body('+', A, B) -> A + B;
path_body('-', A, B) -> A - B;
path_body('*', A, B) -> A * B;
path_body('/', A, B) -> A / B;
path_body('div', A, B) -> A div B;
path_body('rem', A, B) -> A rem B;
path_body('band', A, B) -> A band B;
path_body('bor', A, B) -> A bor B;
path_body('bxor', A, B) -> A bxor B;
path_body('bsl', A, B) -> A bsl B;
path_body('bsr', A, B) -> A bsr B;
path_body('==', A, B) -> A == B;
path_body('/=', A, B) -> A /= B;
path_body('=:=', A, B) -> A =:= B;
path_body('=/=', A, B) -> A =/= B;
path_body('<', A, B) -> A < B;
path_body('=<', A, B) -> A =< B;
path_body('>', A, B) -> A > B;
path_body('>=', A, B) -> A >= B;
path_body(map_get, A, B) -> map_get(A, B).

%% Arithmetic guards match numeric zero, retaining success and rejection paths.
path_guard('+', A, B) when A + B == 0 -> matched;
path_guard('-', A, B) when A - B == 0 -> matched;
path_guard('*', A, B) when A * B == 0 -> matched;
path_guard('/', A, B) when A / B == 0 -> matched;
path_guard('div', A, B) when A div B == 0 -> matched;
path_guard('rem', A, B) when A rem B == 0 -> matched;
path_guard('band', A, B) when A band B == 0 -> matched;
path_guard('bor', A, B) when A bor B == 0 -> matched;
path_guard('bxor', A, B) when A bxor B == 0 -> matched;
path_guard('bsl', A, B) when A bsl B == 0 -> matched;
path_guard('bsr', A, B) when A bsr B == 0 -> matched;
path_guard('==', A, B) when A == B -> matched;
path_guard('/=', A, B) when A /= B -> matched;
path_guard('=:=', A, B) when A =:= B -> matched;
path_guard('=/=', A, B) when A =/= B -> matched;
path_guard('<', A, B) when A < B -> matched;
path_guard('=<', A, B) when A =< B -> matched;
path_guard('>', A, B) when A > B -> matched;
path_guard('>=', A, B) when A >= B -> matched;
path_guard(map_get, A, B) when map_get(A, B) =:= wanted -> matched;
path_guard(_, _, _) -> rejected.

path_map_pattern(Key, Map) ->
    case Map of
        #{Key := Value} -> {matched, Value};
        _ -> no_match
    end.

%% Keep the byte-list wire format, but bound temporary formatting allocations.
print_result(Value) ->
    <<First, Rest/binary>> = term_to_binary(Value),
    io:put_chars(["LYNX_RESULT [", integer_to_list(First)]),
    print_bytes(Rest),
    io:put_chars("]\n").

print_bytes(<<>>) ->
    ok;
print_bytes(Binary) ->
    Size = min(256, byte_size(Binary)),
    <<Chunk:Size/binary, Rest/binary>> = Binary,
    io:put_chars([[$,, integer_to_list(B)] || B <- binary_to_list(Chunk)]),
    print_bytes(Rest).

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

map_sequence(Setup, Steps) ->
    Map = maps:from_list(Setup),
    {sequence_state(map, Map), sequence(map, Steps, Map)}.

dictionary_sequence(Setup, Steps) ->
    erase(),
    try
        lists:foreach(fun({K, V}) -> put(K, V) end, Setup),
        {sequence_state(dictionary, unused), sequence(dictionary, Steps, unused)}
    after
        erase()
    end.

binary_sequence(Setup, Steps) ->
    Binary = list_to_binary(Setup),
    {sequence_state(binary, Binary), sequence(binary, Steps, Binary)}.

sequence(_Kind, [], _State) ->
    [];
sequence(Kind, [Step | Steps], State) ->
    {Result, Next} =
        try sequence_step(Kind, Step, State) of
            {Value, Updated} -> {{ok, Value}, Updated}
        catch
            Class:Reason -> {{exception, Class, Reason}, State}
        end,
    [{Result, sequence_state(Kind, Next)} | sequence(Kind, Steps, Next)].

sequence_state(map, Map) ->
    map_entries(Map);
sequence_state(binary, Binary) ->
    {byte_size(Binary), Binary};
sequence_state(dictionary, _) ->
    Entries = get(),
    Keys = get_keys(),
    {
        length(Entries),
        maps:from_list(Entries),
        length(Keys),
        maps:from_list([{K, present} || K <- Keys])
    }.

sequence_step(map, {put, K, V}, Map) ->
    Next = maps:put(K, V, Map),
    {Next, Next};
sequence_step(map, {update, K, V}, Map) ->
    Next = maps:update(K, V, Map),
    {Next, Next};
sequence_step(map, {get, K}, Map) ->
    {maps:find(K, Map), Map};
sequence_step(map, {erase, K}, Map) ->
    Next = maps:remove(K, Map),
    {Next, Next};
sequence_step(map, {take, K}, Map) ->
    case maps:take(K, Map) of
        error -> {error, Map};
        {V, Next} -> {{V, Next}, Next}
    end;
sequence_step(map, {selected, V}, Map) ->
    Keys = [K || {K, Value} <- maps:to_list(Map), Value =:= V],
    {{length(Keys), maps:from_list([{K, present} || K <- Keys])}, Map};
sequence_step(map, {clear}, Map) ->
    {Map, #{}};
sequence_step(binary, {part, Start, Length}, Binary) ->
    Next = binary:part(Binary, Start, Length),
    {Next, Next};
sequence_step(binary, {append, Bytes}, Binary) ->
    Next = <<Binary/binary, (list_to_binary(Bytes))/binary>>,
    {Next, Next};
sequence_step(binary, {match, Patterns, Options}, Binary) ->
    {binary:match(Binary, Patterns, Options), Binary};
sequence_step(binary, {split, Patterns, Options}, Binary) ->
    {binary:split(Binary, Patterns, Options), Binary};
sequence_step(dictionary, {put, K, V}, State) ->
    {put(K, V), State};
sequence_step(dictionary, {get, K}, State) ->
    {get(K), State};
sequence_step(dictionary, {erase, K}, State) ->
    {erase(K), State};
sequence_step(dictionary, {selected, V}, State) ->
    Keys = get_keys(V),
    {{length(Keys), maps:from_list([{K, present} || K <- Keys])}, State};
sequence_step(dictionary, {clear}, State) ->
    Entries = erase(),
    {{length(Entries), maps:from_list(Entries)}, State}.

%% Enumeration order is unspecified; retain bindings and detect duplicates.
map_entries(Map) ->
    Entries = maps:to_list(Map),
    {length(Entries), maps:from_list(Entries)}.

map_relations(LeftPairs, RightPairs, QueryKeys) ->
    A = maps:from_list(LeftPairs),
    B = maps:from_list(RightPairs),
    PutFoldA = lists:foldl(fun({K, V}, Acc) -> maps:put(K, V, Acc) end, #{}, LeftPairs),
    PutFoldB = lists:foldl(fun({K, V}, Acc) -> maps:put(K, V, Acc) end, #{}, RightPairs),
    Merged = maps:merge(A, B),
    Combine = fun(K, VA, VB) -> {combined, K, VA, VB} end,
    MergedWith = maps:merge_with(Combine, A, B),
    Intersected = maps:intersect(A, B),
    IntersectCombine = fun(K, VA, VB) -> {shared, K, VA, VB} end,
    IntersectedWith = maps:intersect_with(IntersectCombine, A, B),
    Pred = fun(K, V) -> is_integer(K) orelse is_tuple(K) orelse is_map(V) end,
    Filtered = maps:filter(Pred, A),
    AllKeys = maps:keys(A) ++ maps:keys(B) ++ QueryKeys,
    ExactLookupEq = map_exact_lookup_eq(A, B, AllKeys),
    SemanticLookupEq = map_semantic_lookup_eq(A, B, AllKeys),
    {
        {from_list_eq_fold_put, A =:= PutFoldA, B =:= PutFoldB},
        {sizes, map_size(A), map_size(B), map_size(Merged), map_size(Intersected),
            map_size(Filtered)},
        {equality_vs_lookup, {
            A =:= B,
            ExactLookupEq,
            (A =:= B) =:= ExactLookupEq,
            A == B,
            SemanticLookupEq,
            (A == B) =:= SemanticLookupEq
        }},
        {compare, compare(A, B)},
        {entries, map_entries(A), map_entries(B)},
        {per_key, [
            map_key_relation(
                A,
                B,
                K,
                QueryKeys,
                Merged,
                MergedWith,
                Intersected,
                IntersectedWith,
                Filtered,
                Pred
            )
         || K <- QueryKeys
        ]}
    }.

map_exact_lookup_eq(A, B, Keys) ->
    map_size(A) =:= map_size(B) andalso
        lists:all(fun(K) -> maps:find(K, A) =:= maps:find(K, B) end, Keys).

map_semantic_lookup_eq(A, B, Keys) ->
    map_size(A) =:= map_size(B) andalso
        lists:all(
            fun(K) ->
                case {maps:find(K, A), maps:find(K, B)} of
                    {{ok, VA}, {ok, VB}} -> VA == VB;
                    {error, error} -> true;
                    _ -> false
                end
            end,
            Keys
        ).

map_key_relation(
    A, B, K, QueryKeys, Merged, MergedWith, Intersected, IntersectedWith, Filtered, Pred
) ->
    FindA = maps:find(K, A),
    FindB = maps:find(K, B),
    GetA = path_result(fun() -> maps:get(K, A) end),
    MapGetA = path_result(fun() -> erlang:map_get(K, A) end),
    GetDefaultA = maps:get(K, A, '$default'),
    IsKeyA = maps:is_key(K, A),
    IsMapKeyA = erlang:is_map_key(K, A),
    PutVal = {put_val, K},
    PutA = maps:put(K, PutVal, A),
    PutPreservesOthers = lists:all(
        fun(Q) ->
            case Q =:= K of
                true -> maps:find(Q, PutA) =:= {ok, PutVal};
                false -> maps:find(Q, PutA) =:= maps:find(Q, A)
            end
        end,
        QueryKeys
    ),
    UpdVal = {upd_val, K},
    UpdateOutcome =
        case IsKeyA of
            true ->
                UpdatedA = maps:update(K, UpdVal, A),
                {ok, UpdatedA =:= maps:put(K, UpdVal, A), maps:find(K, UpdatedA) =:= {ok, UpdVal}};
            false ->
                path_result(fun() -> maps:update(K, UpdVal, A) end)
        end,
    RemovedA = maps:remove(K, A),
    RemovePreservesOthers = lists:all(
        fun(Q) ->
            case Q =:= K of
                true -> maps:find(Q, RemovedA) =:= error;
                false -> maps:find(Q, RemovedA) =:= maps:find(Q, A)
            end
        end,
        QueryKeys
    ),
    TakeComparison =
        case maps:take(K, A) of
            error -> {error, FindA =:= error};
            {TakenV, TakenMap} -> {ok, FindA =:= {ok, TakenV}, TakenMap =:= RemovedA}
        end,
    ExpectedMergeFind =
        case FindB of
            {ok, _} = Hit -> Hit;
            error -> FindA
        end,
    ExpectedMergeWithFind =
        case {FindA, FindB} of
            {{ok, VA1}, {ok, VB1}} -> {ok, {combined, K, VA1, VB1}};
            {{ok, VA1}, error} -> {ok, VA1};
            {error, {ok, VB1}} -> {ok, VB1};
            {error, error} -> error
        end,
    ExpectedIntersectFind =
        case {FindA, FindB} of
            {{ok, _}, {ok, VB2}} -> {ok, VB2};
            _ -> error
        end,
    ExpectedIntersectWithFind =
        case {FindA, FindB} of
            {{ok, VA3}, {ok, VB3}} -> {ok, {shared, K, VA3, VB3}};
            _ -> error
        end,
    ExpectedFilterFind =
        case FindA of
            {ok, VF} ->
                case Pred(K, VF) of
                    true -> {ok, VF};
                    false -> error
                end;
            error ->
                error
        end,
    {
        K,
        {lookup, FindA, GetA, MapGetA, GetDefaultA, IsKeyA, IsMapKeyA},
        {put_rel, maps:find(K, PutA) =:= {ok, PutVal}, PutPreservesOthers},
        {update_rel, UpdateOutcome},
        {remove_take_rel, RemovePreservesOthers, TakeComparison},
        {merge_rel, maps:find(K, Merged) =:= ExpectedMergeFind,
            maps:find(K, MergedWith) =:= ExpectedMergeWithFind,
            maps:find(K, Intersected) =:= ExpectedIntersectFind,
            maps:find(K, IntersectedWith) =:= ExpectedIntersectWithFind,
            maps:find(K, Filtered) =:= ExpectedFilterFind}
    }.

map_singleton_iteration(Key, Value) ->
    M = #{Key => Value},
    Iter = maps:iterator(M),
    IsValid0 = maps:is_iterator_valid(Iter),
    case maps:next(Iter) of
        {K1, V1, NextIter} ->
            {
                IsValid0,
                K1 =:= Key,
                V1 =:= Value,
                maps:is_iterator_valid(NextIter),
                maps:next(NextIter),
                map_entries(NextIter),
                maps:to_list(M)
            };
        Other ->
            {unexpected, IsValid0, Other}
    end.

map_guards(Key, Input) ->
    {
        case Input of
            M1 when is_map(M1) -> {map, map_size(M1)};
            _ -> not_map
        end,
        case Input of
            M2 when is_map_key(Key, M2) -> has_key;
            _ -> no_key
        end,
        case Input of
            M3 when map_size(M3) > 0 -> nonempty;
            _ -> empty_or_not_map
        end,
        case Input of
            M4 when map_get(Key, M4) =:= wanted -> matched_wanted;
            _ -> rejected
        end,
        path_result(fun() -> call(erlang, is_map, [Input]) end),
        path_result(fun() -> call(erlang, map_size, [Input]) end),
        path_result(fun() -> call(erlang, is_map_key, [Key, Input]) end),
        path_result(fun() -> call(erlang, map_get, [Key, Input]) end)
    }.

list_relations(Xs, Ys, Tail, Probes) ->
    RevXs = lists:reverse(Xs),
    RevYs = lists:reverse(Ys),
    RevXsTail = lists:reverse(Xs, Tail),
    AppendRevTail = RevXs ++ Tail,
    Appended = Xs ++ Ys,
    DiffAfterAppend = Appended -- Xs,
    AsymDiff = (Ys ++ Xs) -- Xs,
    SortedXs = lists:sort(Xs),
    SortedAppended = lists:sort(Appended),
    {
        {lengths, length(Xs), length(Ys), length(Appended), length(SortedXs)},
        {reverse_laws, lists:reverse(RevXs) =:= Xs, RevXsTail =:= AppendRevTail,
            lists:reverse(Appended) =:= (RevYs ++ RevXs)},
        {subtract_laws, DiffAfterAppend =:= Ys, (Xs -- []) =:= Xs, ([] -- Xs) =:= [],
            (Xs -- Xs) =:= [], AsymDiff},
        {sort_laws, SortedXs, is_nondecreasing(SortedXs), lists:sort(SortedXs) =:= SortedXs,
            is_nondecreasing(SortedAppended), lists:sort(RevXs) == SortedXs},
        {probes, [
            {
                P,
                lists:member(P, Xs),
                lists:any(fun(X) -> X =:= P end, Xs),
                lists:member(P, Appended) =:= (lists:member(P, Xs) orelse lists:member(P, Ys)),
                count_exact(P, Xs),
                count_exact(P, RevXs) =:= count_exact(P, Xs),
                count_exact(P, SortedXs) =:= count_exact(P, Xs),
                count_exact(P, Appended) =:= count_exact(P, Xs) + count_exact(P, Ys),
                count_exact(P, Xs -- Ys) =:= erlang:max(0, count_exact(P, Xs) - count_exact(P, Ys)),
                count_semantic(P, Xs),
                count_semantic(P, SortedXs) =:= count_semantic(P, Xs)
            }
         || P <- Probes
        ]}
    }.

is_nondecreasing([]) ->
    true;
is_nondecreasing([_]) ->
    true;
is_nondecreasing([A, B | Rest]) ->
    (A =< B) andalso is_nondecreasing([B | Rest]).

count_exact(Target, List) ->
    lists:foldl(
        fun(Elem, Acc) ->
            case Elem =:= Target of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        List
    ).

count_semantic(Target, List) ->
    lists:foldl(
        fun(Elem, Acc) ->
            case Elem == Target of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        List
    ).

seq_contract(First, Last, Step, ProbeIndices) ->
    Values = lists:seq(First, Last, Step),
    Seq2Matches =
        case Step of
            1 -> path_result(fun() -> lists:seq(First, Last) end) =:= {ok, Values};
            _ -> not_applicable
        end,
    ExpectedCount =
        if
            Step =:= 0 -> 1;
            true -> (Last - First + Step) div Step
        end,
    ProbeChecks = [
        case I >= 0 andalso I < length(Values) of
            true ->
                Nth = lists:nth(I + 1, Values),
                {I, Nth, Nth =:= First + (I * Step)};
            false ->
                {I, out_of_range, path_result(fun() -> lists:nth(I + 1, Values) end)}
        end
     || I <- ProbeIndices
    ],
    {Values, length(Values), ExpectedCount, length(Values) =:= ExpectedCount, Seq2Matches,
        ProbeChecks}.

%% Observe public elements, not the set representation or enumeration order.
%% Exact keys distinguish 1 from 1.0; retaining counts detects duplicates.
set_elements(Set) ->
    Elements = sets:to_list(Set),
    {
        sets:size(Set),
        sets:is_empty(Set),
        length(Elements),
        maps:from_list([{E, present} || E <- Elements])
    }.

set_algebra(Left, Right) ->
    A = sets:from_list(Left, [{version, 2}]),
    B = sets:from_list(Right, [{version, 2}]),
    Union = sets:union(A, B),
    Intersection = sets:intersection(A, B),
    LeftOnly = sets:subtract(A, B),
    RightOnly = sets:subtract(B, A),
    #{
        left => set_elements(A),
        right => set_elements(B),
        union => set_elements(Union),
        intersection => set_elements(Intersection),
        left_only => set_elements(LeftOnly),
        right_only => set_elements(RightOnly),
        subset => {sets:is_subset(A, B), sets:is_subset(B, A)},
        disjoint => {sets:is_disjoint(A, B), sets:is_disjoint(B, A)},
        equal => {sets:is_equal(A, B), sets:is_equal(B, A)},
        relations => {
            sets:is_equal(Union, sets:union(B, A)),
            sets:is_equal(Intersection, sets:intersection(B, A)),
            sets:is_equal(A, sets:union(LeftOnly, Intersection)),
            sets:is_equal(A, sets:intersection(A, Union)),
            sets:is_disjoint(LeftOnly, RightOnly),
            sets:size(Union) + sets:size(Intersection) =:= sets:size(A) + sets:size(B)
        }
    }.

map_fold(Map, valid) ->
    maps:fold(fun(K, V, Acc) -> Acc#{K => V} end, #{}, Map);
map_fold(Map, wrong_arity) ->
    maps:fold(fun(K, V) -> {K, V} end, #{}, Map);
map_fold(Map, counted) ->
    maps:fold(fun(_K, V, {Count, Sum}) -> {Count + 1, Sum + V} end, {0, 0}, Map);
map_fold(Map, throw) ->
    maps:fold(fun(_, _, _) -> throw(folded) end, initial, Map);
map_fold(Map, error) ->
    maps:fold(fun(_, _, _) -> error(folded) end, initial, Map);
map_fold(Map, exit) ->
    maps:fold(fun(_, _, _) -> exit(folded) end, initial, Map);
map_fold(Map, non_function) ->
    call(maps, fold, [not_fun, initial, Map]).

%% Keep every callback's arguments, including duplicates, but not traversal order.
%% Exceptional cases use at most one eligible entry: the first visited key is
%% otherwise unspecified. No function identities escape in badarity reasons.
map_callback(Operation, Args, Mode) ->
    put(map_calls, []),
    Callback =
        case {Operation, Mode} of
            {_, non_function} -> not_fun;
            {_, wrong_arity} -> fun() -> wrong end;
            {merge_with, _} -> fun(K, A, B) -> map_callback_value(Mode, [K, A, B]) end;
            {intersect_with, _} -> fun(K, A, B) -> map_callback_value(Mode, [K, A, B]) end;
            {update_with, _} -> fun(V) -> map_callback_value(Mode, [V]) end;
            {filter, _} -> fun(K, V) -> map_callback_value(Mode, [K, V]) end;
            {filtermap, _} -> fun(K, V) -> map_callback_value(Mode, [K, V]) end
        end,
    CallArgs =
        case {Operation, Args} of
            {update_with, [Key | Rest]} -> [Key, Callback | Rest];
            _ -> [Callback | Args]
        end,
    Outcome =
        try call(maps, Operation, CallArgs) of
            Result -> {ok, Result}
        catch
            error:{badarity, {BadFun, BadArgs}} ->
                {exception, error, {badarity, BadFun =:= Callback, BadArgs}};
            Class:Reason ->
                {exception, Class, Reason}
        end,
    {Outcome, lists:sort(erase(map_calls))}.

map_callback_value(Mode, Args) ->
    put(map_calls, [Args | get(map_calls)]),
    case Mode of
        throw ->
            throw(callback_failed);
        error ->
            error(callback_failed);
        exit ->
            exit(callback_failed);
        nonboolean ->
            not_boolean;
        transform ->
            case Args of
                [keep, _V] -> true;
                [drop, _V] -> false;
                [K, V] -> {true, {K, V}}
            end;
        valid ->
            case Args of
                [V] -> {updated, V};
                [K, A, B] -> {K, A, B};
                [K, V] -> K =:= keep orelse V =:= selected
            end
    end.

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

receive_sequence(Setup, Steps) ->
    receive_prefixes(Setup, Steps, []).

%% Drain only at the end of each fresh prefix, never between its operations.
%% Requeueing snapshots would reset the receive scan and could hide defects.
receive_prefixes(Setup, Steps, Reversed) ->
    Snapshot = isolated(fun() ->
        lists:foreach(fun(M) -> self() ! M end, Setup),
        Result = lists:foldl(
            fun(Step, _Previous) -> receive_step(Step) end, none, lists:reverse(Reversed)
        ),
        {Result, drain()}
    end),
    case Steps of
        [] -> [Snapshot];
        [Step | Rest] -> [Snapshot | receive_prefixes(Setup, Rest, [Step | Reversed])]
    end.

receive_step({send, Message}) ->
    self() ! Message,
    {sent, Message};
receive_step({exact, Wanted}) ->
    receive
        Wanted -> {matched, Wanted}
    after 0 -> timeout
    end;
receive_step({tag, Tag}) ->
    receive
        {Tag, Value} -> {matched, {Tag, Value}}
    after 0 -> timeout
    end;
receive_step({guard, Kind}) ->
    receive_guard(Kind, 0);
receive_step({clauses, Wanted}) ->
    receive
        Wanted -> {matched, Wanted};
        fallback -> {matched, fallback}
    after 0 -> timeout
    end;
receive_step({any}) ->
    receive
        Message -> {matched, Message}
    after 0 -> timeout
    end.

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

function_compare(A, B) ->
    compare(function_value(local_one, A), function_value(local_one, B)).

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
