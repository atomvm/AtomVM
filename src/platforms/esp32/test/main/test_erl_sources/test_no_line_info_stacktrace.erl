%
% This file is part of AtomVM.
%
% Copyright 2026 Gabriel Mancini <gabriel.mancini@gmail.com>
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

-module(test_no_line_info_stacktrace).

-export([start/0, id/1]).

%% Without line information no frame of the stacktrace has any, and building
%% it must still give a list of frames. malloc(0) returns NULL on ESP-IDF.
-compile([no_line_info]).

start() ->
    try
        erlang:error(?MODULE:id(boom))
    catch
        error:boom:Stacktrace -> frames(Stacktrace)
    end.

frames(Stacktrace) when is_list(Stacktrace) ->
    case lists:keyfind(test_no_line_info_stacktrace, 1, Stacktrace) of
        {test_no_line_info_stacktrace, _Function, _Arity, _Location} -> ok;
        false -> {no_frame, Stacktrace}
    end;
frames(Stacktrace) ->
    {not_a_list, Stacktrace}.

id(X) ->
    X.
