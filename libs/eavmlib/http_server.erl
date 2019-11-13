%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(http_server).

-export([start_server/2, reply/3]).

-include("estdlib.hrl").

start_server(Port, Router) ->
    case ?GEN_TCP:listen(Port, []) of
        {ok, ListenSocket} ->
            spawn(fun() -> accept(ListenSocket, Router) end);
        Error ->
            erlang:display(Error)
    end.

accept(ListenSocket, Router) ->
    case ?GEN_TCP:accept(ListenSocket) of
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
                    Method = ?PROPLISTS:get_value(method, Conn),
                    PathTokens = ?PROPLISTS:get_value(uri, Conn),
                    {ok, Module} = find_route(Method, PathTokens, Router),
                    Module:handle_req(Method, split(PathTokens), Conn),
                    loop(Router, {waiting_body, [], RequestData});

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

reply(StatusCode, Reply, Conn) ->
    Socket = ?PROPLISTS:get_value(socket, Conn),
    FullReply = [
        <<"HTTP/1.1 ">>,
        code_to_status_string(StatusCode),
        "\r\nContent-Type: text/html\r\n\r\n",
        Reply
    ],
    ?GEN_TCP:send(Socket, FullReply),
    ?GEN_TCP:close(Socket).

code_to_status_string(200) ->
    <<"200 OK">>;
code_to_status_string(Code) ->
    [erlang:integer_to_binary(Code), <<" NotOK">>].

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
