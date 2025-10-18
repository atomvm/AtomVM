%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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
%% @doc This modules provides an eunit-compatible API for AtomVM generating
%% an output that can be parsed by dut.expect_unity_test_output
%% @end
%%-----------------------------------------------------------------------------
-module(eunit).

-export([
    start/0,
    test/1,
    test/2
]).

-define(DEFAULT_TIMEOUT, 60000).

-type option() :: exact_execution | {exact_execution, boolean()}.
-type test() ::
    fun(() -> term())
    | atom()
    | {module, atom()}
    | {test, ModuleName :: atom(), FunctionName :: atom()}
    | {Line :: pos_integer(), fun(() -> term())}
    | {generator, GenFun :: fun(() -> test())}
    | {generator, ModuleName :: atom(), FunctionName :: atom()}
    | {Title :: string(), test()}
    | [test()].

-record(state, {
    total = 0 :: non_neg_integer(),
    failures = 0 :: non_neg_integer(),
    runner = undefined :: undefined | pid(),
    spawn = false :: boolean(),
    timeout = ?DEFAULT_TIMEOUT :: timeout(),
    instantiator_arg = undefined :: term()
}).

%% @equiv test(Tests, [])
-spec test(test()) -> ok | {error, {failures, pos_integer()}}.
test(Tests) -> test(Tests, []).

%%-----------------------------------------------------------------------------
%% @param   Tests tests or module to run tests from
%% @param   Options options. If `exact_execution' is true and Tests is a module
%%          name, tests from the module with _tests suffix are not collected.
%% @returns ok if all of the tests pass, or the atom fail, if any of the tests
%%          failed.
%% @doc     Run tests print result using io:format in a format compatible with
%%          `dut.expect_unity_test_output'.
%% @end
%%-----------------------------------------------------------------------------
-spec test(Tests :: test(), Options :: [option()]) -> ok | {error, {failures, pos_integer()}}.
test(Tests, Options) ->
    #state{total = Total, failures = Failures} = test(Tests, [], Options, #state{}),
    Outcome =
        case Failures of
            0 -> "OK";
            _ -> "FAIL"
        end,
    io:format("- ~B Tests ~B Failures ~B Ignored ~s\n", [Total, Failures, 0, Outcome]),
    case Failures of
        0 -> ok;
        _ -> {error, {failures, Failures}}
    end.

test([], _Identification, _Options, State) ->
    State;
test([Test | Tail], Identification, Options, AccState) ->
    NewState = test(Test, Identification, Options, AccState),
    test(Tail, Identification, Options, NewState);
test(Module, Identification, Options, State) when is_atom(Module) ->
    test({module, Module}, Identification, Options, State);
test({module, Module}, _Identification, Options, State) ->
    Exports = Module:module_info(exports),
    TestsModule = module_tests(Module, Exports, []),
    ModuleTests = list_to_atom(atom_to_list(Module) ++ "_tests"),
    TestsModuleTests =
        try
            case proplists:get_value(exact_execution, Options, false) of
                true ->
                    [];
                false ->
                    ExportsModuleTests = ModuleTests:module_info(exports),
                    module_tests(ModuleTests, ExportsModuleTests, [])
            end
        catch
            error:undef ->
                []
        end,
    test(TestsModule ++ TestsModuleTests, [{module, Module}], Options, State);
test({generator, Module, Function}, Identification, Options, State) ->
    TestsList = Module:Function(),
    test(TestsList, [{module, Module}, {test_case, Function} | Identification], Options, State);
test({generator, Function}, Identification, Options, State) when is_function(Function) ->
    TestsList = Function(),
    test(TestsList, Identification, Options, State);
test({Title, Tests}, Identification, Options, State) when
    is_list(Title) andalso is_integer(hd(Title))
->
    test(Tests, [{title, Title} | Identification], Options, State);
test(Tuple, Identification, Options, State) when
    is_tuple(Tuple) andalso size(Tuple) > 2 andalso is_list(element(1, Tuple)) andalso
        is_integer(hd(element(1, Tuple)))
->
    [Title | TupleT] = tuple_to_list(Tuple),
    test(list_to_tuple(TupleT), [{title, Title} | Identification], Options, State);
test({Line, Test}, Identification, Options, State) when is_integer(Line) ->
    test(Test, [{line, Line} | Identification], Options, State);
test({test, Module, Function}, Identification, Options, State) when
    is_atom(Module) andalso is_atom(Function)
->
    test(
        fun Module:Function/0,
        [{module, Module}, {test_case, Function} | Identification],
        Options,
        State
    );
test(Function, Identification, Options, #state{runner = undefined} = State0) when
    is_function(Function, 0)
->
    test({spawn, Function}, Identification, Options, State0);
test(Function, Identification, _Options, State) when
    is_function(Function, 0)
->
    run_test(Identification, Function, State);
test(Instantiator, Identification, Options, State) when
    is_function(Instantiator, 1)
->
    test(Instantiator(State#state.instantiator_arg), Identification, Options, State);
test(
    {spawn, Tests},
    Identification,
    Options,
    #state{runner = PreviousRunner, timeout = Timeout} = State0
) ->
    Self = self(),
    {Pid, MonitorRef} = spawn_opt(
        fun() ->
            State1 = test(Tests, Identification, Options, State0#state{runner = self()}),
            Self ! {self(), State1#state{runner = PreviousRunner}}
        end,
        [monitor]
    ),
    receive
        {Pid, Result} ->
            demonitor(MonitorRef, [flush]),
            Result;
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            report({exit, Reason}, Identification),
            State0#state{total = State0#state.total + 1, failures = State0#state.failures + 1}
    after Timeout ->
        report(timeout, Identification),
        State0#state{total = State0#state.total + 1, failures = State0#state.failures + 1}
    end;
test({inorder, Tests}, Identification, Options, State) ->
    test(Tests, Identification, Options, State);
test({setup, Setup, Tests}, Identification, Options, State) ->
    test({setup, Setup, fun(_) -> ok end, Tests}, Identification, Options, State);
test({setup, Setup, TearDown, Tests}, Identification, Options, State) ->
    test({setup, spawn, Setup, TearDown, Tests}, Identification, Options, State);
test(
    {setup, Where, Setup, TearDown, Tests},
    Identification,
    Options,
    #state{runner = undefined} = State0
) ->
    test({spawn, {setup, Where, Setup, TearDown, Tests}}, Identification, Options, State0);
test(
    {setup, Where, Setup, TearDown, Tests},
    Identification,
    Options,
    #state{instantiator_arg = InstantiatorArg0, runner = PreviousRunner} = State0
) ->
    SetupResult = Setup(),
    SubRunner =
        case Where of
            spawn -> undefined;
            local -> PreviousRunner
        end,
    State1 = test(Tests, Identification, Options, State0#state{
        instantiator_arg = SetupResult, runner = SubRunner
    }),
    TearDown(SetupResult),
    State1#state{instantiator_arg = InstantiatorArg0, runner = PreviousRunner};
test({timeout, Timeout, Tests}, Identification, Options, State) ->
    test(Tests, Identification, Options, State#state{timeout = Timeout * 1000}).

module_tests(_Module, [], Acc) ->
    lists:reverse(Acc);
module_tests(Module, [{Func, 0} | Tail], Acc) ->
    case lists:reverse(atom_to_list(Func)) of
        "_tset_" ++ _ ->
            module_tests(Module, Tail, [{generator, Module, Func} | Acc]);
        "tset_" ++ _ ->
            module_tests(Module, Tail, [{test, Module, Func} | Acc]);
        _ ->
            module_tests(Module, Tail, Acc)
    end;
module_tests(Module, [_ | Tail], Acc) ->
    module_tests(Module, Tail, Acc).

report(Progress, Identification) ->
    ModuleString =
        case lists:keyfind(module, 1, Identification) of
            false -> "";
            {module, Module} -> io_lib:format("~s:", [Module])
        end,
    TestCaseString =
        case lists:keyfind(test_case, 1, Identification) of
            false -> "";
            {test_case, TestCase} -> TestCase
        end,
    LineString =
        case lists:keyfind(line, 1, Identification) of
            false -> "";
            {line, Line} -> io_lib:format("~B:", [Line])
        end,
    TitleString =
        case lists:keyfind(title, 1, Identification) of
            false -> "";
            {title, Title} -> io_lib:format(" (~s)", [Title])
        end,
    ProgressString =
        case Progress of
            start -> "...";
            {pass, Result} -> io_lib:format(":PASS:~p", [Result]);
            {fail, Error, StackTrace} -> io_lib:format(":FAIL:~p\n~p", [Error, StackTrace]);
            {exit, Reason} -> io_lib:format(":FAIL:exited with ~p", [Reason]);
            timeout -> ":FAIL:timeout"
        end,
    io:format("~s:~s ~s~s~s\n", [
        ModuleString, LineString, TestCaseString, TitleString, ProgressString
    ]).

run_test(Identification, TestFun, #state{total = Total} = State0) ->
    report(start, Identification),
    try
        Result = TestFun(),
        report({pass, Result}, Identification),
        State0#state{total = Total + 1}
    catch
        error:Error:StackTrace ->
            report({fail, Error, StackTrace}, Identification),
            State0#state{total = Total + 1, failures = State0#state.failures + 1}
    end.

start() ->
    TestModules = lists:foldl(
        fun({ModuleName, _, _}, Acc) ->
            Module = binary_to_atom(iolist_to_binary(ModuleName), utf8),
            ExportsModule = Module:module_info(exports),
            case lists:member({test, 0}, ExportsModule) of
                true -> [Module | Acc];
                false -> Acc
            end
        end,
        [],
        code:all_available()
    ),
    test(TestModules, [exact_execution]).
