%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(disterl).

-export([start/0]).

start() ->
    {ok, _NetKernelPid} = net_kernel:start('atomvm@127.0.0.1', #{name_domain => longnames}),
    io:format("Distribution was started\n"),
    io:format("Node is ~p\n", [node()]),
    net_kernel:set_cookie(<<"AtomVM">>),
    io:format("Cookie is ~s\n", [net_kernel:get_cookie()]),
    register(disterl, self()),
    io:format(
        "This AtomVM node is waiting for 'quit' message, and this process is registered as 'disterl'\n"
    ),
    io:format("On an OTP node with long names distribution, run:\n"),
    io:format("erlang:set_cookie('atomvm@127.0.0.1', 'AtomVM').\n"),
    io:format("{disterl, 'atomvm@127.0.0.1'} ! quit.\n"),
    receive
        quit -> ok
    end.
