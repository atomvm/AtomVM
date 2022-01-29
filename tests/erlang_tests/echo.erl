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

-module(echo).

-export([start/0, puts/1, puts/2]).

puts(String) ->
    puts(get_pid(), String).

puts(Echo, String) ->
    call(Echo, String).

%% Internal operations

call(Echo, Msg) ->
    Echo ! {self(), Msg},
    receive
        Any -> Any
    end.

get_pid() ->
    case whereis(echo) of
        undefined ->
            start();
        Any ->
            Any
    end.

start() ->
    Pid = erlang:open_port({spawn, "echo"}, []),
    erlang:register(echo, Pid),
    Pid.
