%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_native_records_io_lib).

-export([test/0]).

-include("etest.hrl").

-define(FLT(L), lists:flatten(L)).

%% Native records were introduced in OTP 29.
-if(?OTP_RELEASE >= 29).
-record #pair{a, b}.
-record #empty{}.

test() ->
    try #pair{a = 1, b = [x, y]} of
        P -> test(P)
    catch
        % Older atomvm_packbeam versions drop the Recs chunk, so records cannot
        % be built from an AVM file. Make the skip visible instead of passing
        % silently.
        error:{badrecord, pair} ->
            io:format("test_native_records_io_lib: SKIPPED, Recs chunk missing (atomvm_packbeam < 0.8.3)~n"),
            ok
    end.

test(P) ->
    Expected = "#test_native_records_io_lib:pair{a = 1,b = [x,y]}",
    ?ASSERT_MATCH(?FLT(io_lib:format("~w", [P])), Expected),
    ?ASSERT_MATCH(?FLT(io_lib:format("~p", [P])), Expected),
    ?ASSERT_MATCH(?FLT(io_lib:format("~w", [#empty{}])), "#test_native_records_io_lib:empty{}"),
    ?ASSERT_MATCH(
        ?FLT(io_lib:format("~w", [{P, #empty{}}])),
        "{" ++ Expected ++ ",#test_native_records_io_lib:empty{}}"
    ),
    ?ASSERT_MATCH(
        ?FLT(io_lib:format("~w", [#pair{a = #empty{}, b = "ab"}])),
        "#test_native_records_io_lib:pair{a = #test_native_records_io_lib:empty{},b = [97,98]}"
    ),
    ok.
-else.
test() ->
    ok.
-endif.
