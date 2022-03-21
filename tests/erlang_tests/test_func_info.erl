%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(test_func_info).

-export([start/0, id/1, f/3]).

start() ->
    try id(f(id(10), id(4), id(sub))) of
        AnyVal -> AnyVal
    catch
        error:function_clause -> 89;
        _:_ -> 0
    end.

id(X) ->
    X.

f(A, B, sum) when is_integer(A) andalso is_integer(B) ->
    A + B;
f(A, B, mul) when is_integer(A) andalso is_integer(B) ->
    A * B.
