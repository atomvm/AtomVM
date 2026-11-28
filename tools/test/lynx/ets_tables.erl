% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(ets_tables).

-export([ets_table/4]).

slice_bitstring(B, Offset) when is_bitstring(B) ->
    Size = bit_size(B),
    Padding = (8 - (Offset + Size) rem 8) rem 8,
    Parent = <<201:640, 5:Offset, B/bitstring, 0:Padding, 77>>,
    Start = 640 + Offset,
    <<_:Start, Slice:Size/bitstring, _/bitstring>> = Parent,
    Slice.

ets_neg_zero() ->
    erlang:binary_to_term(<<131, 70, 128, 0, 0, 0, 0, 0, 0, 0>>).

normalize_ets_tab(Tab) when is_atom(Tab) ->
    {named, Tab};
normalize_ets_tab(Tab) when is_reference(Tab) ->
    ref.

safe_delete_ets(undefined) ->
    ok;
safe_delete_ets(Tab) ->
    try
        ets:delete(Tab)
    catch
        error:badarg -> ok
    end.

run_ets_step(Tab, Name, Step) ->
    try
        case Step of
            {insert, Obj} ->
                ets:insert(Tab, Obj);
            {insert_new, Obj} ->
                ets:insert_new(Tab, Obj);
            {lookup, Key} ->
                ets:lookup(Tab, Key);
            {insert_neg0, Val} ->
                ets:insert(Tab, {ets_neg_zero(), Val});
            lookup_neg0 ->
                ets:lookup(Tab, ets_neg_zero());
            {lookup_element, Key, Pos} ->
                ets:lookup_element(Tab, Key, Pos);
            {lookup_element4, Key, Pos, Def} ->
                ets:lookup_element(Tab, Key, Pos, Def);
            {member, Key} ->
                ets:member(Tab, Key);
            {update_element, Key, Spec} ->
                ets:update_element(Tab, Key, Spec);
            {update_element4, Key, Spec, Def} ->
                ets:update_element(Tab, Key, Spec, Def);
            {update_counter, Key, Op} ->
                ets:update_counter(Tab, Key, Op);
            {update_counter4, Key, Op, Def} ->
                ets:update_counter(Tab, Key, Op, Def);
            {take, Key} ->
                ets:take(Tab, Key);
            {delete_key, Key} ->
                ets:delete(Tab, Key);
            {delete_object, Obj} ->
                ets:delete_object(Tab, Obj);
            delete_table ->
                ets:delete(Tab);
            dup_named ->
                T = ets:new(Name, [named_table]),
                DupRes = normalize_ets_tab(T),
                true = ets:delete(T),
                DupRes;
            {slice_insert, Off, KeyBin, Val} ->
                ets:insert(Tab, {slice_bitstring(KeyBin, Off), Val});
            {slice_lookup, Off, KeyBin} ->
                ets:lookup(Tab, slice_bitstring(KeyBin, Off));
            {bad_tab_op, Op, BadTab, Args} ->
                run_bad_tab_op(Op, BadTab, Args)
        end
    of
        Res -> {ok, Res}
    catch
        Class:Reason -> {exception, Class, Reason}
    end.

run_bad_tab_op(lookup, BadTab, Key) ->
    ets:lookup(BadTab, Key);
run_bad_tab_op(member, BadTab, Key) ->
    ets:member(BadTab, Key);
run_bad_tab_op(insert, BadTab, Obj) ->
    ets:insert(BadTab, Obj);
run_bad_tab_op(insert_new, BadTab, Obj) ->
    ets:insert_new(BadTab, Obj);
run_bad_tab_op(lookup_element, BadTab, {Key, Pos}) ->
    ets:lookup_element(BadTab, Key, Pos);
run_bad_tab_op(lookup_element4, BadTab, {Key, Pos, Def}) ->
    ets:lookup_element(BadTab, Key, Pos, Def);
run_bad_tab_op(update_element, BadTab, {Key, Spec}) ->
    ets:update_element(BadTab, Key, Spec);
run_bad_tab_op(update_element4, BadTab, {Key, Spec, Def}) ->
    ets:update_element(BadTab, Key, Spec, Def);
run_bad_tab_op(update_counter, BadTab, {Key, Op}) ->
    ets:update_counter(BadTab, Key, Op);
run_bad_tab_op(update_counter4, BadTab, {Key, Op, Def}) ->
    ets:update_counter(BadTab, Key, Op, Def);
run_bad_tab_op(take, BadTab, Key) ->
    ets:take(BadTab, Key);
run_bad_tab_op(delete1, BadTab, _) ->
    ets:delete(BadTab);
run_bad_tab_op(delete2, BadTab, Key) ->
    ets:delete(BadTab, Key);
run_bad_tab_op(delete_object, BadTab, Obj) ->
    ets:delete_object(BadTab, Obj).

run_ets_steps(Tab, Name, Steps) ->
    true = erlang:garbage_collect(),
    Res = [run_ets_step(Tab, Name, S) || S <- Steps],
    true = erlang:garbage_collect(),
    Res.

ets_table(Name, Options0, AccessMode, Steps0) ->
    {Options, Steps} = binary_to_term(term_to_binary({Options0, Steps0})),
    Captured = {captured, Name, Options, AccessMode, <<5:3>>},
    true = erlang:garbage_collect(),
    {CreateOutcome, StepOutcomes} =
        case AccessMode of
            owner ->
                try ets:new(Name, Options) of
                    Tab ->
                        Outcomes =
                            try
                                run_ets_steps(Tab, Name, Steps)
                            after
                                safe_delete_ets(Tab)
                            end,
                        {{ok, normalize_ets_tab(Tab)}, Outcomes}
                catch
                    C:R -> {{exception, C, R}, []}
                end;
            {other_process, Seed} ->
                try ets:new(Name, Options) of
                    Tab ->
                        Outcomes =
                            try
                                case Seed of
                                    [] -> ok;
                                    _ -> true = ets:insert(Tab, Seed)
                                end,
                                Parent = self(),
                                Tag = make_ref(),
                                {Pid, MRef} = spawn_opt(
                                    fun() ->
                                        Parent ! {ets_done, Tag, run_ets_steps(Tab, Name, Steps)}
                                    end,
                                    [monitor]
                                ),
                                StepRes =
                                    receive
                                        {ets_done, Tag, R} -> R
                                    after 5000 ->
                                        exit(Pid, kill),
                                        timeout
                                    end,
                                receive
                                    {'DOWN', MRef, process, Pid, _} -> ok
                                after 5000 ->
                                    demonitor(MRef, [flush])
                                end,
                                StepRes
                            after
                                safe_delete_ets(Tab)
                            end,
                        {{ok, normalize_ets_tab(Tab)}, Outcomes}
                catch
                    C:R -> {{exception, C, R}, []}
                end;
            {owner_exited, Seed} ->
                Parent = self(),
                Tag = make_ref(),
                {Pid, MRef} = spawn_opt(
                    fun() ->
                        try ets:new(Name, Options) of
                            Tab ->
                                case Seed of
                                    [] -> ok;
                                    _ -> true = ets:insert(Tab, Seed)
                                end,
                                Parent ! {ets_created, Tag, {ok, normalize_ets_tab(Tab)}, Tab}
                        catch
                            C:R -> Parent ! {ets_created, Tag, {exception, C, R}, undefined}
                        end
                    end,
                    [monitor]
                ),
                {Created, TabOrUndef} =
                    receive
                        {ets_created, Tag, COut, TVal} -> {COut, TVal}
                    after 5000 ->
                        exit(Pid, kill),
                        {{exception, error, timeout}, undefined}
                    end,
                receive
                    {'DOWN', MRef, process, Pid, _} -> ok
                after 5000 ->
                    demonitor(MRef, [flush])
                end,
                Outcomes =
                    case TabOrUndef of
                        undefined -> [];
                        _ -> run_ets_steps(TabOrUndef, Name, Steps)
                    end,
                {Created, Outcomes}
        end,
    true = erlang:garbage_collect(),
    {CreateOutcome, StepOutcomes, Name, Options, AccessMode, Captured}.
