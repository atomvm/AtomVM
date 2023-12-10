%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Fred Dushin <fred@dushin.net>
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
    BackendOptions =
        case get_otp_version() of
            Version when Version =:= atomvm orelse (is_integer(Version) andalso Version >= 24) ->
                [[{inet_backend, inet}], [{inet_backend, socket}]];
            _ ->
                [[]]
        end,
    [
        ok = test_send_receive(SpawnControllingProcess, IsActive, Mode, BackendOption)
     || SpawnControllingProcess <- [false, true],
        IsActive <- [false, true],
        Mode <- [binary, list],
        BackendOption <- BackendOptions
    ],
    ok.

test_send_receive(SpawnControllingProcess, IsActive, Mode, BackendOption) ->
    io:format("GEN_UDP-TEST> SpawnControllingProcess=~p IsActive=~p Mode=~p Backendoption=~p~n", [
        SpawnControllingProcess, IsActive, Mode, BackendOption
    ]),

    {ok, Socket} = gen_udp:open(0, BackendOption ++ [{active, IsActive}, Mode]),
    {ok, Port} = inet:port(Socket),

    Self = self(),
    F = fun() ->
        case SpawnControllingProcess of
            true -> Self ! ready;
            _ -> ok
        end,
        NumReceived = count_received(Socket, IsActive, Mode),
        case SpawnControllingProcess of
            true ->
                case SpawnControllingProcess of
                    true -> Self ! {done, NumReceived};
                    _ -> ok
                end;
            false ->
                ok
        end,
        NumReceived
    end,

    case SpawnControllingProcess of
        true ->
            Pid = spawn(F),
            gen_udp:controlling_process(Socket, Pid),
            receive
                ready -> ok
            end;
        false ->
            ok
    end,

    NumToSend = 10,
    Sender = erlang:spawn(?MODULE, start_sender, [Socket, Port, make_messages(NumToSend)]),

    NumReceived =
        case SpawnControllingProcess of
            true ->
                receive
                    {done, Received} ->
                        Received
                end;
            false ->
                F()
        end,
    Sender ! stop,

    ?ASSERT_TRUE((0 < NumReceived) and (NumReceived =< NumToSend)),
    %% NB. Might be closed if controlling process terminates
    case SpawnControllingProcess of
        true ->
            catch gen_udp:close(Socket);
        _ ->
            ok = gen_udp:close(Socket)
    end,
    ok.

make_messages(0) ->
    [];
make_messages(N) ->
    [<<"foo">> | make_messages(N - 1)].

start_sender(Socket, Port, Msgs) ->
    send(Socket, Port, Msgs),
    receive
        stop ->
            ok
    end.

send(_Socket, _Port, []) ->
    ok;
send(Socket, Port, [Msg | Rest]) ->
    gen_udp:send(Socket, {127, 0, 0, 1}, Port, Msg),
    send(Socket, Port, Rest).

count_received(_Socket, true = _IsActive, Mode) ->
    count_active_received(Mode, 0);
count_received(Socket, false = _IsActive, Mode) ->
    count_passive_received(Socket, Mode, 0).

count_active_received(Mode, I) ->
    receive
        {udp, _Pid, _Address, _Port, <<"foo">>} when Mode =:= binary ->
            count_active_received(Mode, I + 1);
        {udp, _Pid, _Address, _Port, "foo"} when Mode =:= list ->
            count_active_received(Mode, I + 1);
        Other ->
            erlang:display({count_active_received, unexpected, Other}),
            count_active_received(Mode, I)
    after 500 ->
        I
    end.

count_passive_received(Socket, Mode, I) ->
    case gen_udp:recv(Socket, 0, 500) of
        {ok, {_Address, _Port, <<"foo">>}} when Mode =:= binary ->
            count_passive_received(Socket, Mode, I + 1);
        {ok, {_Address, _Port, "foo"}} when Mode =:= list ->
            count_passive_received(Socket, Mode, I + 1);
        {error, timeout} ->
            I;
        Other ->
            erlang:display({count_passive_received, unexpected, Other}),
            count_passive_received(Socket, Mode, I)
    end.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.
