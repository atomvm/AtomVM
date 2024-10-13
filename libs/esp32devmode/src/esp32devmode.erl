%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(esp32devmode).

-export([
    start_dev_mode/0,
    start_repl/1,
    start_network/0,
    handle_req/3,
    erase_net_config/0,
    save_net_config/2
]).

-record(nc_state, {socket, pending_pid, pending_ref}).

-define(DEFAULT_AP_SSID, <<"AtomVM-ESP32">>).
-define(DEFAULT_AP_PSK, <<"esp32default">>).
-define(DEFAULT_CONSOLE_PORT, 2323).
-define(DEFAULT_WEB_SERVER_PORT, 8080).

start_dev_mode() ->
    avm_pubsub:start(default_pubsub),
    spawn(fun maybe_start_network/0),
    started.

%%
%% Network management
%%

erase_net_config() ->
    io:format("Erasing net config.~n"),
    esp:nvs_erase_key(atomvm, sta_ssid),
    esp:nvs_erase_key(atomvm, sta_psk).

save_net_config(SSID, Pass) ->
    io:format("Saving config: SSID: ~p Pass: ~p.~n", [SSID, Pass]),
    esp:nvs_set_binary(atomvm, sta_ssid, erlang:list_to_binary(SSID)),
    esp:nvs_set_binary(atomvm, sta_psk, erlang:list_to_binary(Pass)).

get_net_config() ->
    case esp:nvs_get_binary(atomvm, sta_ssid) of
        undefined ->
            get_default_net_config();
        SSID ->
            case esp:nvs_get_binary(atomvm, sta_psk) of
                undefined ->
                    get_default_net_config();
                Psk ->
                    get_net_config(SSID, Psk)
            end
    end.

get_default_net_config() ->
    Creds = [
        {ssid, ?DEFAULT_AP_SSID},
        {psk, ?DEFAULT_AP_PSK}
    ],
    {wait_for_ap, Creds}.

get_net_config(SSID, Psk) ->
    Creds = [
        {ssid, SSID},
        {psk, Psk}
    ],
    {wait_for_sta, Creds}.

maybe_start_network() ->
    case esp:nvs_get_binary(atomvm, wlan_enabled) of
        undefined ->
            start_network();
        <<"always">> ->
            start_network();
        <<"never">> ->
            not_started
    end.

start_network() ->
    io:format("Starting network...~n"),
    {WaitFunc, Creds} = get_net_config(),
    case network:WaitFunc(Creds) of
        ok ->
            io:format("WLAN AP ready. Waiting connections.~n"),
            Event = #{
                event => wlan_ap_started
            },
            avm_pubsub:pub(default_pubsub, [system, network, wlan, connected], Event),
            maybe_start_web_server(),
            maybe_start_console(),
            started;
        {ok, {Address, Netmask, Gateway}} ->
            io:format(
                "Acquired IP address: ~s Netmask: ~s Gateway: ~s~n",
                [to_string(Address), to_string(Netmask), to_string(Gateway)]
            ),
            Event = #{
                event => wlan_connected,
                address => Address,
                netmask => Netmask,
                gateway => Gateway
            },
            avm_pubsub:pub(default_pubsub, [system, network, wlan, connected], Event),
            maybe_start_web_server(),
            maybe_start_console(),
            started;
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error]),
            not_started
    end.

to_string({{A, B, C, D}, Port}) ->
    io_lib:format("~p.~p.~p.~p:~p", [A, B, C, D, Port]);
to_string({A, B, C, D}) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]).

%%
%% LISP
%%

maybe_start_console() ->
    case get_console_config() of
        {always, Port} ->
            listen(Port);
        _ ->
            io:format("ALISP console not enabled: skipping.~n"),
            not_started
    end.

listen(Port) ->
    case gen_tcp:listen(Port, []) of
        {ok, ListenSocket} ->
            io:format("ALISP console listening on port ~p~n", [Port]),
            spawn(fun() -> accept(ListenSocket) end),
            started;
        Error ->
            io:format("An error occurred listening: ~p~n", [Error]),
            {error, Error}
    end.

get_console_config() ->
    Enable =
        case esp:nvs_get_binary(atomvm, console_enable) of
            undefined ->
                always;
            <<"always">> ->
                always;
            <<"never">> ->
                never
        end,
    Port =
        case esp:nvs_get_binary(atomvm, console_port) of
            undefined ->
                ?DEFAULT_CONSOLE_PORT;
            PortBinary ->
                try erlang:binary_to_integer(PortBinary) of
                    PortInt -> PortInt
                catch
                    Error ->
                        io:format("Unable to read ALISP console port: ~p.~n", [Error]),
                        ?DEFAULT_CONSOLE_PORT
                end
        end,
    {Enable, Port}.

accept(ListenSocket) ->
    io:format("Waiting to accept shell connection...~n"),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn_opt(?MODULE, start_repl, [self()], [link]),
            io:format("Accepted shell connection. local: ~s peer: ~s~n", [
                local_address(Socket), peer_address(Socket)
            ]),
            spawn(fun() -> accept(ListenSocket) end),
            loop(#nc_state{socket = Socket});
        Error ->
            io:format("An error occurred accepting connection: ~p~n", [Error])
    end.

loop(State) ->
    receive
        {tcp_closed, _Socket} ->
            io:format("Connection closed.~n"),
            erlang:exit(connection_closed);
        {tcp, _Socket, <<255, 244, 255, 253, 6>>} ->
            io:format("Break.~n"),
            gen_tcp:close(State#nc_state.socket),
            erlang:exit(break);
        {tcp, _Socket, Packet} ->
            Reply = {io_reply, State#nc_state.pending_ref, Packet},
            State#nc_state.pending_pid ! Reply,
            loop(State#nc_state{pending_pid = undefined, pending_ref = undefined});
        {io_request, FPid, FRef, Request} ->
            {ok, NewState} = io_request(Request, FPid, FRef, State),
            loop(NewState)
    end.

local_address(Socket) ->
    {ok, SockName} = inet:sockname(Socket),
    to_string(SockName).

peer_address(Socket) ->
    {ok, Peername} = inet:peername(Socket),
    to_string(Peername).

start_repl(SocketIOLeader) ->
    erlang:group_leader(SocketIOLeader, self()),
    arepl:start().

io_request({get_line, unicode, Data}, FPid, FRef, State) ->
    gen_tcp:send(State#nc_state.socket, Data),
    {ok, State#nc_state{pending_pid = FPid, pending_ref = FRef}};
io_request({put_chars, unicode, Data}, FPid, FRef, State) ->
    gen_tcp:send(State#nc_state.socket, Data),
    FPid ! {io_reply, FRef, ok},
    {ok, State}.

%%
%% Web Server
%%

maybe_start_web_server() ->
    case get_web_server_config() of
        {always, Port} ->
            Router = [
                {"*", ?MODULE, []}
            ],
            http_server:start_server(Port, Router),
            io:format("Web server listening on port ~p~n", [Port]),
            started;
        _ ->
            io:format("Web server not enabled: skipping.~n"),
            not_started
    end.

get_web_server_config() ->
    Enable =
        case esp:nvs_get_binary(atomvm, web_server_enable) of
            undefined ->
                always;
            <<"always">> ->
                always;
            <<"never">> ->
                never
        end,
    Port =
        case esp:nvs_get_binary(atomvm, web_server_port) of
            undefined ->
                ?DEFAULT_WEB_SERVER_PORT;
            PortBinary ->
                try erlang:binary_to_integer(PortBinary) of
                    PortInt -> PortInt
                catch
                    Error ->
                        io:format("Unable to read web server port: ~p.~n", [Error]),
                        ?DEFAULT_WEB_SERVER_PORT
                end
        end,
    {Enable, Port}.

handle_req("GET", [], Conn) ->
    Body =
        <<
            "<html>\n"
            "   <body>\n"
            "       <h1>Configuration</h1>\n"
            "       <form method=\"post\">\n"
            "           <p>SSID: <input type=\"text\" name=\"ssid\"></p>\n"
            "           <p>Pass: <input type=\"text\" name=\"pass\"></p>\n"
            "           <input type=\"submit\" value=\"Submit\">\n"
            "       </form>\n"
            "   </body>\n"
            "</html>"
        >>,
    http_server:reply(200, Body, Conn);
handle_req("POST", [], Conn) ->
    ParamsBody = proplists:get_value(body_chunk, Conn),
    Params = http_server:parse_query_string(ParamsBody),

    SSID = proplists:get_value("ssid", Params),
    Pass = proplists:get_value("pass", Params),
    save_net_config(SSID, Pass),

    Body =
        <<
            "<html>\n"
            "   <body>\n"
            "       <h1>Configuration</h1>\n"
            "       <p>Configured, restart device to apply wifi configuration.</p>\n"
            "   </body>\n"
            "</html>"
        >>,
    http_server:reply(200, Body, Conn);
handle_req(Method, Path, Conn) ->
    erlang:display(Conn),
    erlang:display({Method, Path}),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).
