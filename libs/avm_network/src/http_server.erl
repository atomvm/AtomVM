%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Davide Bettio <davide@uninstall.it>
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

-module(http_server).

-export([start_server/2, reply/3, reply/4, parse_query_string/1]).

start_server(Port, Router) ->
    case gen_tcp:listen(Port, []) of
        {ok, ListenSocket} ->
            spawn(fun() -> accept(ListenSocket, Router) end);
        Error ->
            erlang:display(Error)
    end.

accept(ListenSocket, Router) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, _Socket} ->
            spawn(fun() -> accept(ListenSocket, Router) end),
            loop(Router);
        Error ->
            erlang:display(Error)
    end.

loop(Router) ->
    loop(Router, {waiting_request_line, [], []}).

loop(Router, OldStateAndData) ->
    receive
        {tcp_closed, _Socket} ->
            ok;
        {tcp, Socket, Packet} ->
            case parse_http(Packet, OldStateAndData) of
                {got_request, RequestData} ->
                    Conn = [{socket, Socket} | RequestData],
                    Method = proplists:get_value(method, Conn),
                    PathTokens = proplists:get_value(uri, Conn),
                    {ok, Module} = find_route(Method, PathTokens, Router),
                    {ok, _UpdatedConn} = Module:handle_req(Method, split(PathTokens), Conn),
                    ok;
                {need_more, StateAndData} ->
                    loop(Router, StateAndData)
            end
    end.

find_route(_Method, _Path, []) ->
    {error, 404};
find_route(Method, Path, [{Target, Mod, _Opts} | T]) ->
    if
        Path == Target ->
            {ok, Mod};
        Target == "*" ->
            {ok, Mod};
        true ->
            find_route(Method, Path, T)
    end.

reply(StatusCode, ReplyBody, Conn) ->
    {ok, NewConn} = reply(
        StatusCode, ReplyBody, [<<"Content-Type: text/html\r\nConnection: close\r\n">>], Conn
    ),
    Socket = proplists:get_value(socket, NewConn),
    gen_tcp:close(Socket),
    ClosedConn =
        case proplists:get_value(closed, NewConn) of
            undefined -> [{closed, true} | NewConn];
            true -> NewConn
        end,
    {ok, ClosedConn}.

reply(StatusCode, ReplyBody, ReplyHeaders, Conn) ->
    Socket = proplists:get_value(socket, Conn),
    ReplyList =
        case is_list(ReplyBody) of
            true -> ReplyBody;
            false -> [ReplyBody]
        end,
    FullReply = [
        [
            <<"HTTP/1.1 ">>,
            code_to_status_string(StatusCode),
            <<"\r\n">>,
            ReplyHeaders,
            <<"\r\n">>
        ]
        | ReplyList
    ],
    case send_reply(Socket, FullReply) of
        ok ->
            {ok, Conn};
        {error, _} ->
            gen_tcp:close(Socket),
            ClosedConn = [{closed, true} | Conn],
            {ok, ClosedConn}
    end.

send_reply(Socket, [Chunk | Tail]) when is_list(Chunk) orelse is_binary(Chunk) ->
    case gen_tcp:send(Socket, Chunk) of
        ok -> send_reply(Socket, Tail);
        {error, _} = ErrorTuple -> ErrorTuple
    end;
send_reply(_Socket, []) ->
    ok;
send_reply(Socket, IOData) ->
    gen_tcp:send(Socket, IOData).

code_to_status_string(200) ->
    <<"200 OK">>;
code_to_status_string(Code) ->
    [erlang:integer_to_binary(Code), <<" NotOK">>].

parse_query_string(L) ->
    parse_query_string(L, key, [], []).

parse_query_string([$+ | Tail], State, Acc, Data) ->
    parse_query_string([$\s | Tail], State, Acc, Data);
parse_query_string([$%, Hex1, Hex2 | Tail], State, Acc, Data) ->
    Char = (hex_char_to_n(Hex1) bsl 4) bor hex_char_to_n(Hex2),
    parse_query_string([Char | Tail], State, Acc, Data);
parse_query_string([$& | Tail], value, Acc, [LastKey | Data]) ->
    NewData = [{LastKey, reverse(Acc)} | Data],
    parse_query_string(Tail, key, [], NewData);
parse_query_string([$= | Tail], key, Acc, Data) ->
    parse_query_string(Tail, value, [], [reverse(Acc) | Data]);
parse_query_string([], value, Acc, [LastKey | Data]) ->
    [{LastKey, reverse(Acc)} | Data];
parse_query_string([C | Tail], State, Acc, Data) ->
    parse_query_string(Tail, State, [C | Acc], Data).

hex_char_to_n(N) when N >= $0 andalso N =< $9 ->
    N - $0;
hex_char_to_n(N) when N >= $a andalso N =< $f ->
    (N - $a) + 10;
hex_char_to_n(N) when N >= $A andalso N =< $F ->
    (N - $A) + 10.

split(L) ->
    Tokens = split(L, [], []),
    TrimmedTokens =
        case Tokens of
            [[] | T] -> T;
            AlreadyTrimmed -> AlreadyTrimmed
        end,
    [_H | T2] = reverse(TrimmedTokens),
    T2.

split([], TokenAcc, Acc) ->
    [reverse(TokenAcc) | Acc];
split([$/ | T], TokenAcc, Acc) ->
    split(T, [], [reverse(TokenAcc) | Acc]);
split([H | T], TokenAcc, Acc) ->
    split(T, [H | TokenAcc], Acc).

parse_http([], {waiting_body, [], Data}) ->
    {got_request, Data};
parse_http([], {in_body, Acc, Data}) ->
    {got_request, [{body_chunk, reverse(Acc)} | Data]};
parse_http([], StateAndData) ->
    {need_more, StateAndData};
parse_http([$\n | Tail], {{waiting_lf, NextState}, [], Data}) ->
    parse_http(Tail, {NextState, [], Data});
parse_http([C | Tail], {waiting_request_line, [], []}) ->
    parse_http(Tail, {in_method, [C], []});
parse_http([$\s | Tail], {in_method, Acc, []}) ->
    parse_http(Tail, {waiting_uri, [], [{method, reverse(Acc)}]});
parse_http([C | Tail], {in_method, Acc, []}) ->
    parse_http(Tail, {in_method, [C | Acc], []});
parse_http([C | Tail], {waiting_uri, [], Data}) ->
    parse_http(Tail, {in_uri, [C], Data});
parse_http([$\s | Tail], {in_uri, Acc, Data}) ->
    parse_http(Tail, {waiting_http_version, [], [{uri, reverse(Acc)} | Data]});
parse_http([C | Tail], {in_uri, Acc, Data}) ->
    parse_http(Tail, {in_uri, [C | Acc], Data});
parse_http([C | Tail], {waiting_http_version, [], Data}) ->
    parse_http(Tail, {in_http_version, [C], Data});
parse_http([$\r | Tail], {in_http_version, Acc, Data}) ->
    parse_http(Tail, {{waiting_lf, waiting_headers}, [], [{http_version, reverse(Acc)} | Data]});
parse_http([C | Tail], {in_http_version, Acc, Data}) ->
    parse_http(Tail, {in_http_version, [C | Acc], Data});
parse_http([$\r | Tail], {waiting_headers, [], Data}) ->
    parse_http(Tail, {{waiting_lf, waiting_body}, [], Data});
parse_http([C | Tail], {waiting_headers, [], Data}) ->
    parse_http(Tail, {in_header, [C], Data});
parse_http([$\r | Tail], {in_header, Acc, Data}) ->
    parse_http(Tail, {{waiting_lf, waiting_headers}, [], [{header, reverse(Acc)} | Data]});
parse_http([C | Tail], {in_header, Acc, Data}) ->
    parse_http(Tail, {in_header, [C | Acc], Data});
parse_http([C | Tail], {waiting_body, [], Data}) ->
    parse_http(Tail, {in_body, [C], Data});
parse_http([C | Tail], {in_body, Acc, Data}) ->
    parse_http(Tail, {in_body, [C | Acc], Data});
parse_http([C | Tail], {_, _Acc, Data}) ->
    erlang:display({consume, [C]}),
    parse_http(Tail, {consume, [], Data});
parse_http(Input, StateTuple) ->
    erlang:display({cannot_process, Input, StateTuple}).

reverse(L) -> reverse(L, []).

reverse([], Acc) ->
    Acc;
reverse([H | T], Acc) ->
    reverse(T, [H | Acc]).
