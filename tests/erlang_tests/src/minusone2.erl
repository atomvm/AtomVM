%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(minusone2).

-export([start/0, sub/2, id/1]).

start() ->
    OK1 = sub(id(2), id(1)),
    OK2 = sub(id(OK1), id(1)),
    OK3 = sub(id(OK2), id(1)),
    OK4 = sub(id(OK3), id(1)),
    OK5 = sub(id(OK4), id(1)),
    OK6 = sub(id(OK5), id(1)),
    OK6 * sub(id(5), id(1)).

sub(A, B) ->
    try id(A) - id(B) of
        Any -> Any
    catch
        _:_ -> A div (B + 1)
    end.

id(I) ->
    I.
