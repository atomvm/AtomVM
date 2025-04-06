%
% This file is part of AtomVM.
%
% Copyright 2025 migmatore <kazakvova201@gmail.com>
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

-module(unary_plus).

-export([start/0, unary_plus_int/1, unary_plus_float/1, unary_plus_str/1]).

start() ->
    ok = ?MODULE:unary_plus_int(1621436),
    ok = ?MODULE:unary_plus_float(1135.12523),
    ok = ?MODULE:unary_plus_str("string"),
    0.

unary_plus_int(A) ->
    A = +A,
    ok.

unary_plus_float(A) ->
    A = +A,
    ok.

unary_plus_str(A) ->
    try +A of
        ok -> error
    catch
        error:badarith -> ok;
        _:_ -> error
    end.
