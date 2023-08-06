%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Davide Bettio <davide@uninstall.it>
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

-module(system_info_server).

-export([start/0, handle_req/3]).

start() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, "SSID"},
            {psk, "PSK"},
            {connected, fun() -> Self ! connected end},
            {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
            {disconnected, fun() -> Self ! disconnected end}
        ]}
    ],
    case network:start(Config) of
        {ok, _Pid} ->
            wait_for_message();
        Error ->
            io:format("An error occurred starting network: ~p~n", [Error])
    end.

wait_for_message() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    receive
        connected ->
            io:format("Connected~n");
        {ok, IpInfo} ->
            io:format("Acquired IP address: ~p~n", [IpInfo]),
            http_server:start_server(8080, Router);
        disconnected ->
            io:format("Disconnected~n")
    after 15000 ->
        ok
    end,
    wait_for_message().

handle_req("GET", [], Conn) ->
    TimeString = universaltime_to_bin(erlang:universaltime()),
    Body = [<<"<html><body><h1>">>, TimeString, <<"</h1></body></html>">>],
    http_server:reply(200, Body, Conn);
handle_req("GET", ["system", "info"], Conn) ->
    SysInfo = [
        {atom_count, erlang:system_info(atom_count)},
        {process_count, erlang:system_info(process_count)},
        {port_count, erlang:system_info(port_count)},
        {word_size, erlang:system_info(wordsize)},
        {system_architecture, erlang:system_info(system_architecture)}
    ],
    Body = json_encoder:encode(SysInfo),
    http_server:reply(200, Body, Conn);
handle_req("GET", ["processes", PidString, "info"], Conn) ->
    {Code, ProcInfo} = try_proc_info_list(PidString),
    Body = json_encoder:encode(ProcInfo),
    http_server:reply(Code, Body, Conn);
handle_req(Method, Path, Conn) ->
    io:format("Method: ~p Path: ~p~n", [Method, Path]),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).

universaltime_to_bin({{Year, Month, Day}, {H, M, S}}) ->
    [
        erlang:integer_to_binary(Year),
        $/,
        erlang:integer_to_binary(Month),
        $/,
        erlang:integer_to_binary(Day),
        $\s,
        erlang:integer_to_binary(H),
        $:,
        erlang:integer_to_binary(M),
        $:,
        erlang:integer_to_binary(S)
    ].

try_proc_info_list(PidString) ->
    try proc_info_list(PidString) of
        Res -> {200, Res}
    catch
        _:_ -> {404, [{error, <<"Process not found.">>}]}
    end.

proc_info_list(PidString) ->
    PidInteger = erlang:list_to_integer(PidString),
    Procs = erlang:processes(),
    Pid = lists:nth(PidInteger, Procs),
    io:format("pid: ~p~n", [Pid]),
    [
        erlang:process_info(Pid, heap_size),
        erlang:process_info(Pid, stack_size),
        erlang:process_info(Pid, message_queue_len),
        erlang:process_info(Pid, memory)
    ].
