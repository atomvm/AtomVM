%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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

-module(test_system_info).
-export([start/0, loop/1]).

start() ->
    assert(erlang:system_info(process_count) =:= 1),
    assert(erlang:system_info(port_count) =:= 0),
    assert(erlang:system_info(atom_count) > 0),
    assert(erlang:system_info(wordsize) > 0),
    assert(is_binary(erlang:system_info(system_architecture))),
    assert(erlang:system_info(some_wierd_unused_key) =:= undefined),

    Self = self(),
    Pid = spawn(?MODULE, loop, [Self]), receive ok -> ok end,

    assert(erlang:system_info(process_count) =:= 2),

    Pid ! {Self, stop}, receive ok -> 0 end.

loop(undefined) ->
    receive
        {Pid, stop} ->
            Pid ! ok
    end;
loop(Pid) ->
    Pid ! ok,
    loop(undefined).

assert(true) -> ok.
