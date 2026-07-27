%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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

-module(test_run_script_tracked).
-export([start/0]).

start() ->
    try
        {R1, R2} = test_array_script_yields_one_handle_per_element(),
        ok = test_keys_are_distinct_nonneg_integers(R1, R2),
        ok = test_values_are_fetched_in_input_order(R1, R2),
        ok = test_iodata_script_is_accepted(),
        ok = test_empty_array_yields_empty_list(),
        ok = test_null_yields_empty_list(),
        ok = test_throwing_script_is_an_error(),
        ok = test_non_array_script_is_an_error(),
        ok = test_non_iodata_script_raises(),
        ok = test_non_string_value_is_badvalue(),
        ok = test_multibyte_utf8_value_round_trips(),
        ok = test_large_value_round_trips(),
        ok = test_empty_string_value_round_trips(),
        ok = test_many_small_values_round_trip(),
        ok = test_many_large_values_round_trip(),
        Ra = test_deleted_key_is_badkey(),
        ok = test_invalid_get_tracked_args_raise(Ra),
        ok = test_empty_handle_list_yields_empty_list(),
        ok = test_throwing_get_hook_is_whole_call_error(Ra),
        ok = test_wrong_length_get_hook_is_whole_call_error(Ra),
        ok = test_getter_throwing_get_hook_is_whole_call_error(Ra),
        ok = test_invalid_hook_keys_are_an_error(),
        ok = test_duplicate_hook_keys_are_an_error(),
        ok = test_throwing_result_iterator_drops_its_values(),
        ok = test_script_replacing_a_global_still_answers(),
        ok = test_exhausted_key_space_is_an_error(),
        ok = test_garbage_collection_deletes_values(),
        ok = report_success(),
        % Keep R1, R2 and Ra alive: cypress asserts their values are still
        % tracked, and returning from start/0 would tear the runtime down.
        loop([R1, R2, Ra])
    catch
        T:V:S ->
            report_failure(T, V, S)
    end.

test_array_script_yields_one_handle_per_element() ->
    {ok, [R1, R2]} = emscripten:run_script_tracked(<<"['atom', 'vm']">>),
    {R1, R2}.

test_keys_are_distinct_nonneg_integers(R1, R2) ->
    [K1, K2] = emscripten:get_tracked([R1, R2], key),
    true = is_integer(K1) andalso K1 >= 0,
    true = is_integer(K2) andalso K2 >= 0,
    true = K1 =/= K2,
    ok.

test_values_are_fetched_in_input_order(R1, R2) ->
    [{ok, <<"atom">>}, {ok, <<"vm">>}] = emscripten:get_tracked([R1, R2], value),
    ok.

test_iodata_script_is_accepted() ->
    {ok, [_, _]} = emscripten:run_script_tracked(["['io'", ", 'data']"]),
    ok.

test_empty_array_yields_empty_list() ->
    {ok, []} = emscripten:run_script_tracked(<<"[]">>),
    ok.

test_null_yields_empty_list() ->
    {ok, []} = emscripten:run_script_tracked(<<"null">>),
    ok.

test_throwing_script_is_an_error() ->
    {error, badarg} = emscripten:run_script_tracked(<<"throw new Error('boom');">>),
    ok.

test_non_array_script_is_an_error() ->
    {error, badarg} = emscripten:run_script_tracked(<<"42">>),
    ok.

test_non_iodata_script_raises() ->
    expect_badarg(fun() -> emscripten:run_script_tracked(42) end).

test_non_string_value_is_badvalue() ->
    {ok, [RNum]} = emscripten:run_script_tracked(<<"[42]">>),
    [{error, badvalue}] = emscripten:get_tracked([RNum], value),
    ok.

% U+00E9 (2 UTF-8 bytes), U+20AC (3 bytes) and U+1F389 (a surrogate pair in
% the script, 4 UTF-8 bytes) exercise the multibyte paths of the value fetch.
test_multibyte_utf8_value_round_trips() ->
    {ok, [R]} = emscripten:run_script_tracked(<<"['\\u00e9\\u20ac\\ud83c\\udf89']">>),
    [{ok, <<16#C3, 16#A9, 16#E2, 16#82, 16#AC, 16#F0, 16#9F, 16#8E, 16#89>>}] =
        emscripten:get_tracked([R], value),
    ok.

% 80 bytes is past REFC_BINARY_MIN, so this takes the refc binary branch
test_large_value_round_trips() ->
    Str = lists:duplicate(80, $a),
    {ok, [R]} = emscripten:run_script_tracked(["['", Str, "']"]),
    Expected = list_to_binary(Str),
    [{ok, Expected}] = emscripten:get_tracked([R], value),
    ok.

test_empty_string_value_round_trips() ->
    {ok, [R]} = emscripten:run_script_tracked(<<"['']">>),
    [{ok, <<>>}] = emscripten:get_tracked([R], value),
    ok.

% Several values in one call catch an answer heap sized for a single binary:
% each of them takes its own binary on that heap.
test_many_small_values_round_trip() ->
    Strings = [lists:duplicate(10, C) || C <- lists:seq($a, $j)],
    {ok, Refs} = emscripten:run_script_tracked(tracked_script(Strings)),
    Expected = [{ok, list_to_binary(S)} || S <- Strings],
    Expected = emscripten:get_tracked(Refs, value),
    ok.

% values of REFC_BINARY_MIN bytes and more take the refc binary branch
test_many_large_values_round_trip() ->
    Strings = [lists:duplicate(50, C) || C <- lists:seq($a, $h)],
    {ok, Refs} = emscripten:run_script_tracked(tracked_script(Strings)),
    Expected = [{ok, list_to_binary(S)} || S <- Strings],
    Expected = emscripten:get_tracked(Refs, value),
    ok.

tracked_script(Strings) ->
    Quoted = [[$', S, $'] || S <- Strings],
    ["[", lists:join($,, Quoted), "]"].

test_deleted_key_is_badkey() ->
    {ok, [Ra, Rb]} = emscripten:run_script_tracked(<<"['keep', 'drop']">>),
    [_Ka, Kb] = emscripten:get_tracked([Ra, Rb], key),
    ok = emscripten:run_script(
        [<<"window.Module.trackedObjectsMap.delete(">>, integer_to_list(Kb), <<");">>],
        [main_thread]
    ),
    [{ok, <<"keep">>}, {error, badkey}] = emscripten:get_tracked([Ra, Rb], value),
    Ra.

test_invalid_get_tracked_args_raise(Ra) ->
    ok = expect_badarg(fun() -> emscripten:get_tracked(not_a_list, key) end),
    ok = expect_badarg(fun() -> emscripten:get_tracked([Ra], bogus) end),
    ok = expect_badarg(fun() -> emscripten:get_tracked([1, 2], key) end),
    ok = expect_badarg(fun() -> emscripten:get_tracked([Ra | Ra], key) end),
    ok.

% the {ok, []} of a script that tracked nothing must be usable as input
test_empty_handle_list_yields_empty_list() ->
    [] = emscripten:get_tracked([], key),
    [] = emscripten:get_tracked([], value),
    {ok, []} = emscripten:run_script_tracked(<<"[]">>),
    ok.

% a throwing override must yield the whole-call error, not hang the caller
test_throwing_get_hook_is_whole_call_error(Ra) ->
    ok = emscripten:run_script(
        <<
            "window.Module.savedOnGetTrackedObjects = window.Module.onGetTrackedObjects;"
            "window.Module.onGetTrackedObjects = function() { throw new Error('hook boom'); };"
        >>,
        [main_thread]
    ),
    [{error, badvalue}] = emscripten:get_tracked([Ra], value),
    ok = restore_get_hook(),
    [{ok, <<"keep">>}] = emscripten:get_tracked([Ra], value),
    ok.

test_wrong_length_get_hook_is_whole_call_error(Ra) ->
    ok = emscripten:run_script(
        <<
            "window.Module.savedOnGetTrackedObjects = window.Module.onGetTrackedObjects;"
            "window.Module.onGetTrackedObjects = function() { return []; };"
        >>,
        [main_thread]
    ),
    [{error, badvalue}] = emscripten:get_tracked([Ra], value),
    ok = restore_get_hook(),
    ok.

% The hook returns without throwing here: the throw comes from reading an
% element of the array it returned, which must not escape the guard either.
test_getter_throwing_get_hook_is_whole_call_error(Ra) ->
    ok = emscripten:run_script(
        <<
            "window.Module.savedOnGetTrackedObjects = window.Module.onGetTrackedObjects;"
            "window.Module.onGetTrackedObjects = function(keys) {"
            "  const result = new Array(keys.length);"
            "  Object.defineProperty(result, 0, {"
            "    get: function() { throw new Error('getter boom'); }"
            "  });"
            "  return result;"
            "};"
        >>,
        [main_thread]
    ),
    [{error, badvalue}] = emscripten:get_tracked([Ra], value),
    ok = restore_get_hook(),
    [{ok, <<"keep">>}] = emscripten:get_tracked([Ra], value),
    ok.

% A key outside the unsigned 32 bit range must fail the call rather than be
% truncated into an unrelated entry, and every value the hook tracked must be
% dropped again, under the malformed key as well as the valid one.
test_invalid_hook_keys_are_an_error() ->
    ok = expect_invalid_hook_key_error(<<"-1">>),
    ok = expect_invalid_hook_key_error(<<"1.5">>),
    ok = expect_invalid_hook_key_error(<<"4294967296">>),
    ok = expect_invalid_hook_key_error(<<"4294967295">>),
    ok.

expect_invalid_hook_key_error(InvalidKey) ->
    ok = install_invalid_key_hook(InvalidKey),
    {error, badarg} = emscripten:run_script_tracked(<<"ignored by the hook">>),
    ok = restore_run_hook(),
    {ok, [RValid, RBad]} = emscripten:run_script_tracked(
        <<
            "[String(window.Module.trackedObjectsMap.has(window.validHookKey)),"
            " String(window.Module.trackedObjectsMap.has(window.badHookKey))]"
        >>
    ),
    [{ok, <<"false">>}, {ok, <<"false">>}] = emscripten:get_tracked([RValid, RBad], value),
    ok.

install_invalid_key_hook(InvalidKey) ->
    emscripten:run_script(
        [
            <<"window.Module.savedOnRunTrackedJs = window.Module.onRunTrackedJs;">>,
            <<"window.Module.onRunTrackedJs = function() {">>,
            <<"  const key = window.Module.nextTrackedObjectKey();">>,
            <<"  window.Module.trackedObjectsMap.set(key, 'orphan');">>,
            <<"  window.validHookKey = key;">>,
            <<"  const bad = ">>,
            InvalidKey,
            <<";">>,
            <<"  window.Module.trackedObjectsMap.set(bad, 'orphan');">>,
            <<"  window.badHookKey = bad;">>,
            <<"  return [key, bad];">>,
            <<"};">>
        ],
        [main_thread]
    ).

% An evaluated script shares its realm with the module and can replace a
% global, through an undeclared assignment in sloppy mode if nothing else.
% Two values are needed: a single one never reaches the duplicate check.
test_script_replacing_a_global_still_answers() ->
    {ok, [R1, R2]} = emscripten:run_script_tracked(
        <<
            "window.savedSet = window.Set;"
            "window.Set = function () { throw new Error('clobbered'); };"
            "['one', 'two'];"
        >>
    ),
    ok = emscripten:run_script(
        <<"window.Set = window.savedSet; delete window.savedSet;">>,
        [main_thread]
    ),
    [{ok, <<"one">>}, {ok, <<"two">>}] = emscripten:get_tracked([R1, R2], value),
    ok.

% Reading the returned array can throw after it has yielded elements: the
% default hook has tracked them by then and has to give them up itself.
test_throwing_result_iterator_drops_its_values() ->
    {error, badarg} = emscripten:run_script_tracked(
        <<
            "const values = [];"
            "values[Symbol.iterator] = function* () {"
            "  yield 'iterated';"
            "  throw new Error('iterator boom');"
            "};"
            "values;"
        >>
    ),
    {ok, [R]} = emscripten:run_script_tracked(
        <<
            "[String([...window.Module.trackedObjectsMap.values()]"
            ".includes('iterated'))]"
        >>
    ),
    [{ok, <<"false">>}] = emscripten:get_tracked([R], value),
    ok.

% One key returned twice would get one handle each, and the first collected
% would delete the value the other still addresses. The rollback must drop it
% once: a deletion hook is not required to be idempotent.
test_duplicate_hook_keys_are_an_error() ->
    ok = emscripten:run_script(
        <<
            "window.Module.savedOnTrackedObjectDelete = window.Module.onTrackedObjectDelete;"
            "window.duplicateDeleteCount = 0;"
            "window.Module.onTrackedObjectDelete = function(key) {"
            "  if (key === window.duplicateHookKey) {"
            "    window.duplicateDeleteCount += 1;"
            "  }"
            "  window.Module.savedOnTrackedObjectDelete(key);"
            "};"
            "window.Module.savedOnRunTrackedJs = window.Module.onRunTrackedJs;"
            "window.Module.onRunTrackedJs = function() {"
            "  const key = window.Module.nextTrackedObjectKey();"
            "  window.Module.trackedObjectsMap.set(key, 'twice');"
            "  window.duplicateHookKey = key;"
            "  return [key, key];"
            "};"
        >>,
        [main_thread]
    ),
    {error, badarg} = emscripten:run_script_tracked(<<"ignored by the hook">>),
    ok = restore_run_hook(),
    ok = emscripten:run_script(
        <<
            "window.Module.onTrackedObjectDelete = window.Module.savedOnTrackedObjectDelete;"
            "delete window.Module.savedOnTrackedObjectDelete;"
        >>,
        [main_thread]
    ),
    {ok, [RHas, RCount]} = emscripten:run_script_tracked(
        <<
            "[String(window.Module.trackedObjectsMap.has(window.duplicateHookKey)),"
            " String(window.duplicateDeleteCount)]"
        >>
    ),
    [{ok, <<"false">>}, {ok, <<"1">>}] = emscripten:get_tracked([RHas, RCount], value),
    ok.

% The key counter saturates at the reserved key rather than wrapping into a
% key a live handle still owns, and the default hook refuses to track under it.
test_exhausted_key_space_is_an_error() ->
    ok = emscripten:run_script(
        <<
            "window.Module.savedNextTrackedObjectKey = window.Module.nextTrackedObjectKey;"
            "window.Module.nextTrackedObjectKey = function() { return 4294967295; };"
        >>,
        [main_thread]
    ),
    {error, badarg} = emscripten:run_script_tracked(<<"['exhausted']">>),
    ok = emscripten:run_script(
        <<
            "window.Module.nextTrackedObjectKey = window.Module.savedNextTrackedObjectKey;"
            "delete window.Module.savedNextTrackedObjectKey;"
        >>,
        [main_thread]
    ),
    {ok, [R]} = emscripten:run_script_tracked(
        <<"[String(window.Module.trackedObjectsMap.has(4294967295))]">>
    ),
    [{ok, <<"false">>}] = emscripten:get_tracked([R], value),
    ok.

restore_run_hook() ->
    emscripten:run_script(
        <<
            "window.Module.onRunTrackedJs = window.Module.savedOnRunTrackedJs;"
            "delete window.Module.savedOnRunTrackedJs;"
        >>,
        [main_thread]
    ).

restore_get_hook() ->
    emscripten:run_script(
        <<
            "window.Module.onGetTrackedObjects = window.Module.savedOnGetTrackedObjects;"
            "delete window.Module.savedOnGetTrackedObjects;"
        >>,
        [main_thread]
    ).

% Handles dropped on the Erlang side must disappear from the JS map once
% garbage collected, while kept handles must survive; asserted by cypress
% through window.gcBaseline and the final map contents. Only the final
% state can be asserted: the VM may collect dropped handles at any
% allocation point, not just at explicit erlang:garbage_collect() calls.
test_garbage_collection_deletes_values() ->
    erlang:garbage_collect(),
    ok = emscripten:run_script(
        <<"window.gcBaseline = window.Module.trackedObjectsMap.size;">>,
        [main_thread]
    ),
    ok = make_garbage(),
    erlang:garbage_collect(),
    ok.

% The handles must go out of scope before garbage collection: create them
% in their own stack frame and drop them on return.
make_garbage() ->
    {ok, [_, _, _]} = emscripten:run_script_tracked(<<"['g1', 'g2', 'g3']">>),
    ok.

expect_badarg(Fun) ->
    try
        Result = Fun(),
        {no_error_raised, Result}
    catch
        error:badarg -> ok;
        T:V -> {unexpected_error, T, V}
    end.

report_success() ->
    emscripten:run_script(
        [<<"window.document.getElementById('result').innerHTML = 'Test success';">>],
        [main_thread]
    ).

report_failure(T, V, S) ->
    emscripten:run_script(
        [
            <<"window.document.getElementById('result').innerHTML = \"Failure: ">>,
            escape_js_str(lists:flatten(io_lib:format("~p\n~p\n~p", [T, V, S]))),
            <<"\";">>
        ],
        [main_thread, async]
    ).

loop(KeepRefs) ->
    receive
        _Any -> loop(KeepRefs)
    end.

escape_js_str(Str) ->
    escape_js_str(Str, []).

escape_js_str([$\\ | Tail], Acc) ->
    escape_js_str(Tail, ["\\\\" | Acc]);
escape_js_str([$" | Tail], Acc) ->
    escape_js_str(Tail, ["\\\"" | Acc]);
escape_js_str([$\n | Tail], Acc) ->
    escape_js_str(Tail, ["<br />" | Acc]);
escape_js_str([$& | Tail], Acc) ->
    escape_js_str(Tail, ["&amp;" | Acc]);
escape_js_str([$< | Tail], Acc) ->
    escape_js_str(Tail, ["&lt;" | Acc]);
escape_js_str([$> | Tail], Acc) ->
    escape_js_str(Tail, ["&gt;" | Acc]);
escape_js_str([C | Tail], Acc) ->
    escape_js_str(Tail, [C | Acc]);
escape_js_str([], Acc) ->
    lists:reverse(Acc).
