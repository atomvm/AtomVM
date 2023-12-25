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

-module(ceilfloat).

-export([start/0]).

start() ->
    to_int(id(id([-2.5, 0]))).

to_int(A) ->
    try ceil(id(A)) of
        B -> B
    catch
        error:badarg -> -1;
        _:_ -> 1
    end.

id([I | _T]) -> id(I);
id(I) -> I.
