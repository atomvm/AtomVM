%
% This file is part of AtomVM.
%
% Copyright 2017-2024 Davide Bettio <davide@uninstall.it>
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

-module(selval).

-export([start/0, selval/1, as_float/1, selval2/1, safe_selval2/1]).

start() ->
    ?MODULE:selval(2) - ?MODULE:selval2(?MODULE:as_float(<<"5.0">>)) +
        ?MODULE:safe_selval2(1).

selval(0) ->
    7;
selval(1) ->
    8;
selval(2) ->
    9;
selval(7) ->
    10;
selval(_) ->
    0.

as_float(X) ->
    binary_to_float(X).

selval2(1.0) -> -1;
selval2(2.0) -> -2;
selval2(3.0) -> -3;
selval2(4.0) -> -4;
selval2(5.0) -> 9;
selval2(6.0) -> -6;
selval2(7.0) -> -7;
selval2(8.0) -> -8.

safe_selval2(X) ->
    try selval2(X) of
        _R -> -1000
    catch
        error:function_clause -> 0;
        _:_ -> -2000
    end.
