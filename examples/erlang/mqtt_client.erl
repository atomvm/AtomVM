%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(mqtt_client).

-export([start/0, parse/1, publish/2, disconnect_req/0]).

-define(MQTT_BROKER, "test.mosquitto.org").

start() ->
    tcp_client_start().

parse(<<16#20, MsgLen, 0, ReturnCode>>) ->
    {connack, MsgLen, ReturnCode};
parse(<<16#90, MsgLen, MsgId:16/integer-unsigned-big, QoS>>) ->
    {suback, MsgLen, MsgId, QoS};
parse(<<16#30, _MsgLen, TopicLen:16/integer-unsigned-big, TopicAndMsg/binary>>) ->
    <<Topic:TopicLen/binary, Msg/binary>> = TopicAndMsg,
    {publish, Topic, Msg};
parse(Binary) ->
    Binary.

publish(Topic, Message) ->
    TopicLen = byte_size(Topic),
    MsgLen = 2 + TopicLen + byte_size(Message),
    <<16#30, MsgLen, TopicLen:16/integer-unsigned-big, Topic/binary, Message/binary>>.

disconnect_req() ->
    <<16#E0, 16#00>>.

connect_command() ->
    ProtocolName = <<"MQIsdp">>,
    ProtocolNameLen = byte_size(ProtocolName),
    ProtocolVersion = 3,
    Flags = 2,
    KeepAlive = 60,
    ClientID = <<"atomvm|mqtt-test">>,
    ClientIDLen = byte_size(ClientID),
    MsgLen = 2 + ProtocolNameLen + 1 + 1 + 2 + 2 + ClientIDLen,
    <<16#10, MsgLen, ProtocolNameLen:16/integer-unsigned-big, ProtocolName/binary, ProtocolVersion,
        Flags, KeepAlive:16/integer-unsigned-big, ClientIDLen:16/integer-unsigned-big,
        ClientID/binary>>.

subscribe_command(Topic) ->
    TopicLen = byte_size(Topic),
    QoS = 0,
    MsgLen = 2 + 2 + TopicLen + 1,
    MsgId = 1,
    <<16#82, MsgLen, MsgId:16/integer-unsigned-big, TopicLen:16/integer-unsigned-big, Topic/binary,
        QoS>>.

tcp_client_start() ->
    erlang:display("Connecting"),
    case gen_tcp:connect(?MQTT_BROKER, 1883, [{active, true}, {binary, true}]) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, connect_command()) of
                ok ->
                    active_receive_data();
                Error ->
                    io:format("An error occurred sending a packet: ~p~n", [Error])
            end;
        Error ->
            io:format("An error occurred connecting: ~p~n", [Error])
    end.

active_receive_data() ->
    receive
        {tcp_closed, _Socket} ->
            erlang:display("Connection closed."),
            ok;
        {tcp, Socket, Packet} ->
            case parse(Packet) of
                {connack, _MsgLen, ReturnCode} ->
                    erlang:display({"Received connack.", ReturnCode}),
                    gen_tcp:send(Socket, publish(<<"/test/atomvm">>, <<"Hello MQTT">>)),
                    gen_tcp:send(Socket, subscribe_command(<<"/test/atomvm">>));
                {suback, _MsgLen, MsgId, QoS} ->
                    erlang:display({"Suback received.", MsgId, QoS});
                {publish, Topic, Message} ->
                    erlang:display({publish, Topic, Message});
                Binary ->
                    erlang:display(Binary)
            end,

            active_receive_data()
    end.
