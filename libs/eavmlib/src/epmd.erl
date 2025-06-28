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

-module(epmd).

-behaviour(gen_server).

-export([start_link/1]).

% gen_server API
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(NAMES_REQ, 110).
-define(ALIVE2_X_RESP, 118).
-define(PORT2_RESP, 119).
-define(ALIVE2_REQ, 120).
-define(ALIVE2_RESP, 121).
-define(PORT_PLEASE2_REQ, 122).

-define(EPMD_DEFAULT_PORT, 4369).
-type epmd_config_option() :: {port, non_neg_integer()}.
-type epmd_config() :: [epmd_config_option()].

-spec start_link(Config :: epmd_config()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%
%% gen_server callbacks
%%

-record(state, {
    socket :: any(),
    port :: non_neg_integer(),
    accept_handle :: undefined | reference(),
    recv_selects :: [tuple()],
    clients :: [{binary(), non_neg_integer(), reference(), binary()}],
    creation :: non_neg_integer()
}).

%% @hidden
init(Config) ->
    Port = proplists:get_value(port, Config, ?EPMD_DEFAULT_PORT),
    {ok, Socket} = socket:open(inet, stream, tcp),
    ok = socket:setopt(Socket, {socket, reuseaddr}, true),
    ok = socket:bind(Socket, #{
        family => inet,
        port => Port,
        addr => {0, 0, 0, 0}
    }),
    ok = socket:listen(Socket),
    RandCreation = 42,
    State0 = #state{
        socket = Socket, port = Port, recv_selects = [], clients = [], creation = RandCreation
    },
    State1 = accept(State0),
    {ok, State1}.

%% @hidden
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(
    {'$socket', _Socket, abort, {Ref, closed}},
    #state{clients = Clients0, recv_selects = RecvSelects0} = State
) ->
    Clients1 = lists:keydelete(Ref, 3, Clients0),
    RecvSelects1 = lists:keydelete(Ref, 1, RecvSelects0),
    {noreply, State#state{clients = Clients1, recv_selects = RecvSelects1}};
handle_info({'$socket', Socket, select, Ref}, #state{socket = Socket, accept_handle = Ref} = State) ->
    NewState = accept(State),
    {noreply, NewState};
handle_info(
    {'$socket', Socket, select, Ref},
    #state{clients = Clients0, recv_selects = RecvSelects0} = State
) ->
    NewState =
        case lists:keyfind(Ref, 1, RecvSelects0) of
            {Ref, client} ->
                socket:close(Socket),
                Clients1 = lists:keydelete(Ref, 3, Clients0),
                RecvSelects1 = lists:keydelete(Ref, 1, RecvSelects0),
                State#state{clients = Clients1, recv_selects = RecvSelects1};
            {Ref, req_size} ->
                RecvSelects1 = lists:keydelete(Ref, 1, RecvSelects0),
                client_socket_recv_req_size(Socket, State#state{recv_selects = RecvSelects1});
            {Ref, req, Size} ->
                RecvSelects1 = lists:keydelete(Ref, 1, RecvSelects0),
                client_socket_recv_req(Socket, Size, State#state{recv_selects = RecvSelects1});
            false ->
                State
        end,
    {noreply, NewState}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

accept(#state{socket = Socket} = State) ->
    case socket:accept(Socket, nowait) of
        {select, {select_info, accept, Ref}} ->
            State#state{accept_handle = Ref};
        {ok, ClientSocket} ->
            State1 = client_socket_recv_req_size(ClientSocket, State),
            accept(State1)
    end.

client_socket_recv_req_size(Socket, #state{recv_selects = RecvSelects} = State) ->
    case socket:recv(Socket, 2, nowait) of
        {select, {select_info, recv, Ref}} ->
            State#state{recv_selects = [{Ref, req_size} | RecvSelects]};
        {ok, <<Size:16>>} ->
            client_socket_recv_req(Socket, Size, State)
    end.

client_socket_recv_req(Socket, Size, #state{recv_selects = RecvSelects} = State) ->
    case socket:recv(Socket, Size, nowait) of
        {select, {select_info, recv, Ref}} ->
            State#state{recv_selects = [{Ref, req, Size} | RecvSelects]};
        {ok, Data} ->
            process_req(Data, Socket, State)
    end.

process_req(
    <<?ALIVE2_REQ, Port:16, NodeType, Protocol, HighestVersion:16, LowestVersion:16, NameLen:16,
        Name:NameLen/binary, ExtraLen:16, ExtraData:ExtraLen/binary>>,
    Socket,
    #state{clients = Clients, recv_selects = RecvSelects, creation = Creation} = State
) ->
    case socket:recv(Socket, 1, nowait) of
        {select, {select_info, recv, Ref}} ->
            socket:send(Socket, <<?ALIVE2_X_RESP, 0, Creation:32>>),
            State#state{
                recv_selects = [{Ref, client} | RecvSelects],
                clients = [
                    {Name, Port, Ref,
                        <<Port:16, NodeType, Protocol, HighestVersion:16, LowestVersion:16,
                            NameLen:16, Name:NameLen/binary, ExtraLen:16,
                            ExtraData:ExtraLen/binary>>}
                    | Clients
                ],
                creation = (Creation + 1) rem 16#ffffffff
            };
        {ok, <<_Byte>>} ->
            socket:close(Socket),
            State;
        {error, closed} ->
            State
    end;
process_req(<<?PORT_PLEASE2_REQ, Name/binary>>, Socket, #state{clients = Clients} = State) ->
    case lists:keyfind(Name, 1, Clients) of
        false ->
            ok = socket:send(Socket, <<?PORT2_RESP, 1>>);
        {Name, _Port, _Ref, Data} ->
            ok = socket:send(Socket, <<?PORT2_RESP, 0, Data/binary>>)
    end,
    socket:close(Socket),
    State;
process_req(<<?NAMES_REQ>>, Socket, #state{clients = Clients, port = Port} = State) ->
    ok = socket:send(Socket, <<Port:32>>),
    lists:foreach(
        fun({NodeName, NodePort, _Ref, _Data}) ->
            Line = iolist_to_binary(io_lib:format("name ~ts at port ~p~n", [NodeName, NodePort])),
            ok = socket:send(Socket, Line)
        end,
        Clients
    ),
    socket:close(Socket),
    State.
