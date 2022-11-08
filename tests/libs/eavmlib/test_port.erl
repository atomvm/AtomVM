%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(test_port).

-export([test/0, loop/0]).

test() ->
    {Pid, Ref} = spawn_opt(?MODULE, loop, [], [monitor]),

    pong = port:call(Pid, ping),
    pong = port:call(Pid, ping, 1000),
    out_of_memory = port:call(Pid, out_of_memory),
    out_of_memory = port:call(Pid, out_of_memory, 1000),

    {error, timeout} = port:call(Pid, {sleep, 500}, 250),

    ok = port:call(Pid, halt),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    end,
    {error, noproc} = port:call(Pid, ping),
    {error, noproc} = port:call(Pid, ping, 1000),

    ok.

loop() ->
    receive
        {Pid, Ref, ping} ->
            Pid ! {Ref, pong},
            loop();
        {Pid, _Ref, out_of_memory} ->
            Pid ! out_of_memory,
            loop();
        {Pid, Ref, {sleep, Ms}} ->
            receive
            after Ms -> ok
            end,
            Pid ! {Ref, ok},
            loop();
        {Pid, Ref, halt} ->
            Pid ! {Ref, ok};
        Garbage ->
            erlang:display({unexpected_message, Garbage}),
            loop()
    end.
