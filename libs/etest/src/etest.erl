%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2018 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-----------------------------------------------------------------------------
%% @doc This modules provides a basic testing framework for AtomVM Erlang
%% libraries.
%% @end
%%-----------------------------------------------------------------------------
-module(etest).

-export([test/1]).
-export([assert_match/2, assert_true/1, assert_failure/2]).

%%-----------------------------------------------------------------------------
%% @param   Tests a list of test modules
%% @returns ok if all of the tests pass, or the atom fail, if any of the tests
%%          failed.
%% @doc     Test a sequence of test modules.
%%
%%          This function will execute the test/0 function for each module
%%          provided in the input list of test modules.  If all of the tests
%%          return the atom ok, then this function returns ok.  If any of the
%%          test modules return a value other than ok, then this function
%%          returns the atom fail.
%% @end
%%-----------------------------------------------------------------------------
-spec test(list(module())) -> ok | fail.
test(Tests) ->
    Results = [{Test, run_test(Test)} || Test <- Tests],
    console:puts("\n"),
    erlang:display(Results),
    check_results(Results).

%%-----------------------------------------------------------------------------
%% @param   X a term
%% @param   Y a term
%% @returns ok if X and Y unify; fail otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec assert_match(term(), term()) -> ok | fail.
assert_match(X, X) -> ok;
assert_match(_, _) -> fail.

%%-----------------------------------------------------------------------------
%% @param   X a term
%% @returns ok if X is true; fail otherwise.
%% @end
%%-----------------------------------------------------------------------------
-spec assert_true(boolean()) -> ok | fail.
assert_true(true) -> ok;
assert_true(_) -> fail.

%%-----------------------------------------------------------------------------
%% @param   F a function to evaluate
%% @returns ok if evaluating F results in Error being thrown; fail, otherwise
%% @end
%%-----------------------------------------------------------------------------
-spec assert_failure(fun(), Error::atom()) -> ok | fail.
assert_failure(F, _E) ->
    try
        F(),
        fail
    catch
        %% TODO implement opcode 108 (raise/2)
        _:E ->
            id(E),
            ok
    end.

%%=============================================================================
%% internal operations

%% @private
run_test(Test) ->
    try
        Result = Test:test(),
        console:puts("+"), console:flush(),
        Result
    catch
        _:_ ->
            console:puts("-"), console:flush(),
            exception
    end.

%% @private
check_results([]) ->
    ok;
check_results([{_Test, ok} | T]) ->
    check_results(T);
check_results(_) ->
    fail.


id(X) -> X.
