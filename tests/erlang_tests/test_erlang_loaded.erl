%
% This file is part of AtomVM.
%
% Copyright 2026 AtomVM Contributors
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

-module(test_erlang_loaded).

-export([start/0]).

start() ->
    Loaded = erlang:loaded(),
    true = is_list(Loaded),
    true = length(Loaded) > 0,
    true = contains(?MODULE, Loaded),
    true = all_atoms(Loaded),
    0.

contains(_Module, []) -> false;
contains(Module, [Module | _]) -> true;
contains(Module, [_H | Tail]) -> contains(Module, Tail).

all_atoms([]) -> true;
all_atoms([H | Tail]) -> is_atom(H) andalso all_atoms(Tail).
