%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(export_test_module).

-export([exported_func/1, catching_func/2, tracing_func/1, raising_func/1, erroring_func/1]).

exported_func(0) ->
    1;
exported_func(N) ->
    ?MODULE:exported_func(N - 1) * N.

catching_func(Other, N) ->
    try Other:raising_func(N) of
        R -> {?MODULE, unexpected, R}
    catch
        throw:{thrown, M, X} -> {?MODULE, M, X * 2}
    end.

raising_func(N) ->
    X = id(N) * 2,
    throw({thrown, ?MODULE, X}).

erroring_func(N) ->
    X = id(N) * 2,
    error({my_error, ?MODULE, X}).

tracing_func(Other) ->
    try Other:erroring_func(1) of
        R -> {?MODULE, unexpected, R}
    catch
        error:{my_error, M, _X}:Stacktrace -> {?MODULE, M, Stacktrace}
    end.

id(X) -> X.
