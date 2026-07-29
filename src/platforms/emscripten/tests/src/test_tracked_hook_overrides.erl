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

%% Exercises the tracked values API against overridden hooks rather than the
%% defaults. The overrides live in test_tracked_hook_overrides.html, shaped
%% like an embedder's: the script is a function receiving the module, values
%% are serialized to JSON, keys may be allocated by the hook, and deleting a
%% key runs a cleanup closure first.
-module(test_tracked_hook_overrides).
-export([start/0]).

start() ->
    try
        ok = wait_for_hooks(200),
        ok = test_function_script_yields_one_handle_per_element(),
        ok = test_values_come_back_as_json(),
        ok = test_hook_allocated_key_is_kept(),
        ok = test_unserializable_value_fails_every_element(),
        ok = test_arguments_are_deserialized_by_the_hook(),
        Kept = test_cleanup_runs_when_a_handle_is_collected(),
        ok = report_success(),
        % Returning from start/0 would tear the runtime down, and cypress
        % still has to read the final state.
        loop([Kept])
    catch
        T:V:S ->
            report_failure(T, V, S)
    end.

%% The page installs its hooks right after the module factory resolves, but the
%% browser main thread may drain a proxied call while it is still suspended on
%% that await. The default hook refuses a script evaluating to a function.
wait_for_hooks(0) ->
    {error, hooks_never_installed};
wait_for_hooks(Attempts) ->
    Probed =
        try
            emscripten:run_script_tracked(<<"(Module) => ['ready']">>)
        catch
            T:V -> {caught, T, V}
        end,
    case Probed of
        {ok, [_]} ->
            ok;
        _ ->
            receive
            after 50 -> ok
            end,
            wait_for_hooks(Attempts - 1)
    end.

test_function_script_yields_one_handle_per_element() ->
    Created = emscripten:run_script_tracked(<<"(Module) => { return ['one', 'two']; }">>),
    {ok, Refs} = Created,
    2 = length(Refs),
    ok.

%% The hook serializes with JSON.stringify, so a string comes back quoted
%% and a map comes back as an object.
test_values_come_back_as_json() ->
    Created = emscripten:run_script_tracked(<<"(Module) => [{x: 1}, 'str', 42]">>),
    {ok, Refs} = Created,
    Values = emscripten:get_tracked(Refs, value),
    [{ok, <<"{\"x\":1}">>}, {ok, <<"\"str\"">>}, {ok, <<"42">>}] = Values,
    ok.

%% A hook may hand back a key it allocated itself instead of letting the
%% default one do it. The VM must accept it and report it unchanged.
test_hook_allocated_key_is_kept() ->
    Created = emscripten:run_script_tracked(
        <<
            "(Module) => {"
            "  const key = Module.nextTrackedObjectKey();"
            "  window.preallocatedKey = key;"
            "  return [new TrackedValue({key: key, value: 'preallocated'})];"
            "}"
        >>
    ),
    {ok, Refs} = Created,
    [Key] = emscripten:get_tracked(Refs, key),
    Values = emscripten:get_tracked(Refs, value),
    [{ok, <<"\"preallocated\"">>}] = Values,
    Reported = emscripten:run_script_tracked(
        <<"(Module) => [String(window.preallocatedKey)]">>
    ),
    {ok, ReportedRefs} = Reported,
    % the hook serializes, so the key comes back as a JSON string
    Expected = list_to_binary("\"" ++ integer_to_list(Key) ++ "\""),
    [{ok, Expected}] = emscripten:get_tracked(ReportedRefs, value),
    ok.

%% JSON.stringify throws on a circular structure, which is a hook contract
%% violation: the fetch then says nothing about any single key.
test_unserializable_value_fails_every_element() ->
    Created = emscripten:run_script_tracked(
        <<"(Module) => { const o = {}; o.self = o; return [o, 'plain']; }">>
    ),
    {ok, Refs} = Created,
    Values = emscripten:get_tracked(Refs, value),
    [{error, badvalue}, {error, badvalue}] = Values,
    ok.

%% Embedders pass arguments by embedding them in the script they build.
test_arguments_are_deserialized_by_the_hook() ->
    Created = emscripten:run_script_tracked([
        <<"(Module) => { const args = Module.deserialize('">>,
        <<"{\"n\": 21}">>,
        <<"'); return [String(args.n * 2)]; }">>
    ]),
    {ok, Refs} = Created,
    Values = emscripten:get_tracked(Refs, value),
    [{ok, <<"\"42\"">>}] = Values,
    ok.

%% Collecting a handle must reach the override, which runs the closure
%% registered for that key before dropping the entry. Returns a handle that
%% must NOT be collected, so cypress can tell the two apart.
test_cleanup_runs_when_a_handle_is_collected() ->
    ok = emscripten:run_script(<<"window.cleanupRan = 0;">>, [main_thread]),
    Kept = make_kept_handle(),
    ok = make_collectable_handle(),
    erlang:garbage_collect(),
    ok = wait_for_cleanup(100),
    Kept.

make_kept_handle() ->
    Created = emscripten:run_script_tracked(<<"(Module) => ['kept']">>),
    {ok, [Ref]} = Created,
    Ref.

%% The handle has to go out of scope before the collection, so it is made
%% in a frame of its own and dropped on return.
make_collectable_handle() ->
    Created = emscripten:run_script_tracked(
        <<
            "(Module) => {"
            "  const key = Module.nextTrackedObjectKey();"
            "  Module.cleanupFunctions.set(key, () => { window.cleanupRan += 1; });"
            "  return [new TrackedValue({key: key, value: 'collectable'})];"
            "}"
        >>
    ),
    {ok, [_]} = Created,
    ok.

wait_for_cleanup(0) ->
    {error, cleanup_never_ran};
wait_for_cleanup(Attempts) ->
    Probed = emscripten:run_script_tracked(<<"(Module) => [String(window.cleanupRan)]">>),
    {ok, Refs} = Probed,
    Fetched = emscripten:get_tracked(Refs, value),
    case Fetched of
        [{ok, <<"\"0\"">>}] ->
            receive
            after 50 -> ok
            end,
            wait_for_cleanup(Attempts - 1);
        [{ok, <<"\"1\"">>}] ->
            ok
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
