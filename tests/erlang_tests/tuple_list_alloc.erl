%
% This file is part of AtomVM.
%
% Copyright 2023 Illya Petrov <ilya.muromec@gmail.com>
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

-module(tuple_list_alloc).

-export([start/0]).

start() -> 
  erlang:garbage_collect(self()),
  Config = make_config(1, 2, 3),
  tuple_size(Config).

make_config(A, B, C) -> 
  erlang:garbage_collect(self()),

  X = [
    {
      1, 2, 3, 4, 5, 6, 7, 8, 9, 0,
      1, 2, 3, 4, 5, 6, 7, 8, 9, 0,
      1, 2, 3, 4, 5, 5, 7,
      fun() -> 0 end
    }
  ],
  maketuple(A, B, C, X).

maketuple(A, B, C, D) -> {A, B, C, D}.
