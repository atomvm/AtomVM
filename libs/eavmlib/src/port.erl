%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
% Copyright 2021 Fred Dushin <fred@dushin.net>
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

-module(port).

-export([call/2, call/3]).

-spec call(pid(), Message :: term()) -> term().
call(Pid, Message) ->
    case erlang:is_process_alive(Pid) of
        false ->
            {error, noproc};
        true ->
            Ref = erlang:make_ref(),
            Pid ! {self(), Ref, Message},
            receive
                out_of_memory -> out_of_memory;
                {Ref, Reply} -> Reply
            end
    end.

-spec call(pid(), Message :: term(), TimeoutMs :: non_neg_integer()) -> term() | {error, timeout}.
call(Pid, Message, TimeoutMs) ->
    case erlang:is_process_alive(Pid) of
        false ->
            {error, noproc};
        true ->
            Ref = erlang:make_ref(),
            Pid ! {self(), Ref, Message},
            receive
                out_of_memory -> out_of_memory;
                {Ref, Reply} -> Reply
            after TimeoutMs ->
                {error, timeout}
            end
    end.
