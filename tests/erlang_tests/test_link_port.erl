%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_link_port).

-export([start/0]).

start() ->
    PreviousProcessFlag = process_flag(trap_exit, true),
    Port = open_port({spawn, "echo"}, []),
    link(Port),
    {links, Links} = process_info(self(), links),
    true = list_member(Links, Port),
    exit(Port, kill),
    killed =
        receive
            {'EXIT', Port, Reason} -> Reason
        after 2000 -> timeout
        end,
    process_flag(trap_exit, PreviousProcessFlag),
    0.

list_member([H | _], H) -> true;
list_member([_ | T], H) -> list_member(T, H);
list_member([], _) -> false.
