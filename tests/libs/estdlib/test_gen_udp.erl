%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(test_gen_udp).

-export([test/0, start_sender/3]).

-include("etest.hrl").

test() ->
    ok = test_send_receive_active(),
    ok.

test_send_receive_active() ->
    {ok, Socket} = gen_udp:open(0, [{active, true}]),
    {ok, Port} = inet:port(Socket),
    NumToSend = 10,
    Sender = erlang:spawn(?MODULE, start_sender, [Socket, Port, make_messages(NumToSend)]),
    NumReceived = count_received(),
    Sender ! stop,
    ?ASSERT_TRUE((0 < NumReceived) and (NumReceived =< NumToSend)),
    ok = gen_udp:close(Socket),
    ok.


make_messages(0) ->
    [];
make_messages(N) ->
    [<<"foo">> | make_messages(N - 1)].

start_sender(Socket, Port, Msgs) ->
    send(Socket, Port, Msgs),
    receive stop ->
        ok
    end.

send(_Socket, _Port, []) ->
    ok;
send(Socket, Port, [Msg | Rest]) ->
    gen_udp:send(Socket, {127,0,0,1}, Port, Msg),
    send(Socket, Port, Rest).

count_received() ->
    count_received(0).

count_received(I) ->
    receive
        _Msg ->
            count_received(I + 1)
    after 100 ->
        I
    end.
