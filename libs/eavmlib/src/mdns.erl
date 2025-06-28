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

-module(mdns).

-export([
    start_link/1,
    stop/1
]).

% gen_server API
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

% unit test exports
-export([
    parse_dns_message/1,
    parse_dns_name/2,
    serialize_dns_message/1,
    serialize_dns_name/1
]).

-define(MDNS_PORT, 5353).
-define(MDNS_MULTICAST_ADDR, {224, 0, 0, 251}).
-define(DEFAULT_TTL, 900).

-type config() :: #{hostname := iodata(), interface := inet:ip4_address(), ttl => pos_integer()}.

%% @doc Start mdns server and resolve `Hostname'.local
-spec start_link(Config :: config()) ->
    {ok, pid()} | {error, Reason :: term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Stop mdns responder
-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%
%% gen_server callbacks
%%

-record(state, {
    socket :: any(),
    name :: binary(),
    select_ref :: reference() | undefined,
    self_addr :: map(),
    ttl :: pos_integer()
}).

%% @hidden
init(Config) ->
    Interface = maps:get(interface, Config),
    Hostname = maps:get(hostname, Config),
    TTL = maps:get(ttl, Config, ?DEFAULT_TTL),
    {ok, Socket} = socket:open(inet, dgram, udp),
    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:setopt(Socket, {ip, add_membership}, #{
        multiaddr => ?MDNS_MULTICAST_ADDR, interface => Interface
    }),
    % With esp-idf 5.4, we need to bind the socket to ANY, binding to
    % interface doesn't work.
    % TODO: investigate and maybe file a bug about it
    SelfAddrAny = #{
        family => inet,
        port => ?MDNS_PORT,
        addr => {0, 0, 0, 0}
    },
    ok = socket:bind(Socket, SelfAddrAny),
    SelfAddr = maps:put(addr, Interface, SelfAddrAny),
    State0 = #state{
        socket = Socket,
        name = iolist_to_binary(Hostname),
        select_ref = undefined,
        self_addr = SelfAddr,
        ttl = TTL
    },
    State1 = socket_recvfrom(State0),
    {ok, State1}.

%% @hidden
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({'$socket', Socket, select, Ref}, #state{socket = Socket, select_ref = Ref} = State) ->
    NewState = socket_recvfrom(State),
    {noreply, NewState}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

socket_recvfrom(#state{socket = Socket} = State) ->
    case socket:recvfrom(Socket, 0, nowait) of
        {select, {select_info, recvfrom, Ref}} ->
            State#state{select_ref = Ref};
        {ok, {From, Data}} ->
            process_datagram(State, From, Data),
            socket_recvfrom(State)
    end.

-define(DNS_TYPE_A, 1).
-define(DNS_CLASS_IN, 1).
-define(DNS_OPCODE_STANDARD_QUERY, 0).
-define(DNS_QR_QUERY, 0).
-define(DNS_QR_REPLY, 1).
-define(MDNS_SEND_BROADCAST_RESPONSE, 0).
-define(MDNS_SEND_UNICAST_RESPONSE, 1).

-record(dns_question, {
    qname :: [unicode:latin1_chardata()],
    qtype :: non_neg_integer(),
    unicast_response :: ?MDNS_SEND_BROADCAST_RESPONSE | ?MDNS_SEND_UNICAST_RESPONSE,
    qclass :: non_neg_integer()
}).

-record(dns_rrecord, {
    name :: [unicode:latin1_chardata()],
    type :: non_neg_integer(),
    class :: non_neg_integer(),
    ttl :: non_neg_integer(),
    rdata :: binary()
}).

-record(dns_message, {
    id :: non_neg_integer(),
    qr :: ?DNS_QR_QUERY | ?DNS_QR_REPLY,
    opcode :: 0..15,
    aa :: 0..1,
    questions = [] :: [#dns_question{}],
    answers = [] :: [#dns_rrecord{}],
    authority_rr = [] :: [#dns_rrecord{}],
    additional_rr = [] :: [#dns_rrecord{}]
}).
parse_dns_message(
    <<ID:16, QR:1, Opcode:4, AA:1, _TC:1, _RD:1, _RA:1, 0:3, _RCode:4, QDCount:16, ANCount:16,
        NSCount:16, ARCount:16, Tail/binary>> = Message
) ->
    case parse_dns_questions(Message, QDCount, Tail) of
        {ok, Questions, Rest} ->
            case parse_dns_rrecords(Message, ANCount + NSCount + ARCount, Rest) of
                {ok, RRecords0, <<>>} ->
                    {AnswersRecords, RRecords1} = lists:split(ANCount, RRecords0),
                    {AuthorityRecords, AdditionalRecords} = lists:split(NSCount, RRecords1),
                    {ok, #dns_message{
                        id = ID,
                        qr = QR,
                        aa = AA,
                        opcode = Opcode,
                        questions = Questions,
                        answers = AnswersRecords,
                        authority_rr = AuthorityRecords,
                        additional_rr = AdditionalRecords
                    }};
                {ok, _RRecords, <<_, _/binary>> = Rest} ->
                    {error, {extra_bytes_in_dns_message, Rest}};
                {error, _} = ErrorT0 ->
                    ErrorT0
            end;
        {error, _} = ErrorT1 ->
            ErrorT1
    end;
parse_dns_message(Other) ->
    {error, {invalid_dns_header, Other}}.

parse_dns_questions(Message, Count, Data) ->
    parse_dns_questions(Message, Count, Data, []).

parse_dns_questions(_Message, 0, Data, Acc) ->
    {ok, lists:reverse(Acc), Data};
parse_dns_questions(Message, N, Data, Acc) ->
    case parse_dns_name(Message, Data) of
        {ok, {QName, <<QType:16, UnicastResponse:1, QClass:15, Tail/binary>>}} ->
            parse_dns_questions(Message, N - 1, Tail, [
                #dns_question{
                    qname = QName,
                    qtype = QType,
                    unicast_response = UnicastResponse,
                    qclass = QClass
                }
                | Acc
            ]);
        {ok, _} ->
            {error, {invalid_question, Data}};
        {error, _} = ErrorT ->
            ErrorT
    end.

parse_dns_rrecords(Message, Count, Data) ->
    parse_dns_rrecords(Message, Count, Data, []).

parse_dns_rrecords(_Message, 0, Data, Acc) ->
    {ok, lists:reverse(Acc), Data};
parse_dns_rrecords(Message, N, Data, Acc) ->
    case parse_dns_name(Message, Data) of
        {ok,
            {Name,
                <<Type:16, _CacheFlush:1, Class:15, TTL:32, RDLength:16, RData:RDLength/binary,
                    Tail/binary>>}} ->
            parse_dns_rrecords(Message, N - 1, Tail, [
                #dns_rrecord{name = Name, type = Type, class = Class, ttl = TTL, rdata = RData}
                | Acc
            ]);
        {ok, _} ->
            {error, {invalid_rrecord, Data}};
        {error, _} = ErrorT ->
            ErrorT
    end.

parse_dns_name(Message, Data) ->
    parse_dns_name(Message, Data, []).

parse_dns_name(_Message, <<0, Tail/binary>>, Acc) ->
    {ok, {lists:reverse(Acc), Tail}};
parse_dns_name(Message, <<3:2, Ptr:14, Tail/binary>>, Acc) when byte_size(Message) > Ptr ->
    {_, PtrBin} = split_binary(Message, Ptr),
    case parse_dns_name(Message, PtrBin, Acc) of
        {ok, {Name, _OtherTail}} -> {ok, {Name, Tail}};
        {error, _} = ErrorT -> ErrorT
    end;
parse_dns_name(Message, <<N, Name:N/binary, Rest/binary>>, Acc) when N < 64 ->
    parse_dns_name(Message, Rest, [Name | Acc]);
parse_dns_name(_Message, Other, _Acc) ->
    {error, {invalid_name, Other}}.

% Ignore messages from self.
process_datagram(#state{self_addr = SelfAddr}, SelfAddr, _Data) ->
    ok;
process_datagram(#state{} = State, From, Data) ->
    case parse_dns_message(Data) of
        {ok, #dns_message{
            id = ID, qr = ?DNS_QR_QUERY, opcode = ?DNS_OPCODE_STANDARD_QUERY, questions = Questions
        }} ->
            lists:foreach(
                fun(Question) ->
                    process_question(State, From, ID, Question)
                end,
                Questions
            );
        {ok, _} ->
            ok;
        {error, _} ->
            ok
    end.

process_question(
    #state{name = Name, socket = Socket, self_addr = SelfAddr, ttl = TTL},
    From,
    ID,
    #dns_question{
        qname = [Hostname, Domain],
        qtype = ?DNS_TYPE_A,
        unicast_response = UnicastResponse,
        qclass = ?DNS_CLASS_IN
    } = Question
) ->
    case
        string:to_lower(binary_to_list(Domain)) =:= "local" andalso
            string:to_lower(binary_to_list(Hostname)) =:= string:to_lower(binary_to_list(Name))
    of
        true ->
            % This is our name.
            {IP1, IP2, IP3, IP4} = maps:get(addr, SelfAddr),
            Answer = #dns_message{
                id = ID,
                qr = ?DNS_QR_REPLY,
                aa = 1,
                opcode = ?DNS_OPCODE_STANDARD_QUERY,
                questions = [Question],
                answers = [
                    #dns_rrecord{
                        name = [Name, <<"local">>],
                        type = ?DNS_TYPE_A,
                        class = ?DNS_CLASS_IN,
                        ttl = TTL,
                        rdata = <<IP1, IP2, IP3, IP4>>
                    }
                ]
            },
            AnswerBin = serialize_dns_message(Answer),
            case UnicastResponse of
                ?MDNS_SEND_UNICAST_RESPONSE ->
                    socket:sendto(Socket, AnswerBin, From);
                ?MDNS_SEND_BROADCAST_RESPONSE ->
                    socket:sendto(Socket, AnswerBin, #{
                        family => inet, addr => ?MDNS_MULTICAST_ADDR, port => ?MDNS_PORT
                    })
            end;
        false ->
            ok
    end;
process_question(_State, _From, _ID, _DNSQuestion) ->
    ok.

serialize_dns_message(#dns_message{
    id = ID,
    qr = QR,
    opcode = Opcode,
    aa = AA,
    questions = Questions,
    answers = Answers,
    authority_rr = AuthorityRR,
    additional_rr = AdditionalRR
}) ->
    QuestionsBin = [serialize_dns_question(Question) || Question <- Questions],
    RRecordsBin = [
        serialize_dns_rrecord(RRecord)
     || RRecord <- Answers ++ AuthorityRR ++ AdditionalRR
    ],
    list_to_binary([
        <<ID:16, QR:1, Opcode:4, AA:1, 0:1, 0:1, 0:1, 0:3, 0:4, (length(Questions)):16,
            (length(Answers)):16, (length(AuthorityRR)):16, (length(AdditionalRR)):16>>,
        QuestionsBin,
        RRecordsBin
    ]).

serialize_dns_question(#dns_question{qname = Name, qtype = QType, qclass = QClass}) ->
    NameBin = serialize_dns_name(Name),
    <<NameBin/binary, QType:16, QClass:16>>.

serialize_dns_rrecord(#dns_rrecord{
    name = Name, type = Type, class = Class, ttl = TTL, rdata = RData
}) ->
    NameBin = serialize_dns_name(Name),
    <<NameBin/binary, Type:16, Class:16, TTL:32, (byte_size(RData)):16, RData/binary>>.

serialize_dns_name(Name) ->
    serialize_dns_name(Name, []).

serialize_dns_name([], Acc) ->
    list_to_binary(lists:reverse([<<0>> | Acc]));
serialize_dns_name([Name | Tail], Acc) ->
    serialize_dns_name(Tail, [<<(byte_size(Name)), Name/binary>> | Acc]).
