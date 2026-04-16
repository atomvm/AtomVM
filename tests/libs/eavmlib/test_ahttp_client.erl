%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(test_ahttp_client).
-export([test/0]).

test() ->
    ok = test_passive(),
    ok = test_active(),
    ok = test_passive_socket(),
    ok = test_active_socket(),
    ok = test_chunked_passive(),
    ok = test_chunked_active(),
    ok = test_chunked_split_across_segments(),
    ok = test_chunked_extension(),
    ok = test_chunked_trailer(),
    ok = test_bad_transfer_encoding(),
    ok = test_bad_transfer_encoding_stacked(),
    ok = test_bad_transfer_encoding_bad_order(),
    ok = test_content_length_and_transfer_encoding(),
    ok = test_transfer_encoding_before_content_length(),
    ok = test_chunked_trailer_framing_filtered(),
    ok.

test_passive() ->
    ok = ssl:start(),
    ConnectResult = ahttp_client:connect(https, "test.atomvm.org", 443, [
        {active, false}, {verify, verify_none}, {parse_headers, [<<"Location">>]}
    ]),
    case ConnectResult of
        {ok, Conn} ->
            case ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined) of
                {ok, Conn2, _Ref} ->
                    ok = loop_passive(Conn2, #{});
                {error, _} = RequestError ->
                    io:format("Request failed: ~p~n", [RequestError]),
                    RequestError
            end;
        {error, _} = ConnectError ->
            io:format("Request failed: ~p~n", [ConnectError]),
            ConnectError
    end,
    ok = ssl:stop(),
    ok.

test_active() ->
    ConnectResult = ahttp_client:connect(http, "test.atomvm.org", 80, [{active, true}]),
    case ConnectResult of
        {ok, Conn} ->
            case ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined) of
                {ok, Conn2, _Ref} ->
                    loop_active(Conn2, #{});
                {error, _} = RequestError ->
                    io:format("Request failed: ~p~n", [RequestError]),
                    RequestError
            end;
        {error, _} = ConnectError ->
            io:format("Request failed: ~p~n", [ConnectError]),
            ConnectError
    end.

test_passive_socket() ->
    ConnectResult = ahttp_client:connect(http, "test.atomvm.org", 80, [
        {inet_backend, socket},
        {active, false},
        {parse_headers, [<<"Location">>]}
    ]),
    case ConnectResult of
        {ok, Conn} ->
            case ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined) of
                {ok, Conn2, _Ref} ->
                    loop_passive(Conn2, #{});
                {error, _} = RequestError ->
                    io:format("Request failed: ~p~n", [RequestError]),
                    RequestError
            end;
        {error, _} = ConnectError ->
            io:format("Request failed: ~p~n", [ConnectError]),
            ConnectError
    end.

test_active_socket() ->
    ConnectResult = ahttp_client:connect(http, "test.atomvm.org", 80, [
        {inet_backend, socket}, {active, true}
    ]),
    case ConnectResult of
        {ok, Conn} ->
            case ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined) of
                {ok, Conn2, _Ref} ->
                    loop_active(Conn2, #{});
                {error, _} = RequestError ->
                    io:format("Request failed: ~p~n", [RequestError]),
                    RequestError
            end;
        {error, _} = ConnectError ->
            io:format("Request failed: ~p~n", [ConnectError]),
            ConnectError
    end.

loop_active(Conn, Resp) ->
    receive
        Message ->
            case ahttp_client:stream(Conn, Message) of
                {ok, _Conn, closed} ->
                    #{done := true} = Resp,
                    ok;
                {ok, UpdatedConn, Responses} ->
                    case parse_responses(Responses, Resp, #{status => 200}) of
                        #{done := true} ->
                            ahttp_client:close(Conn),
                            ok;
                        UpdatedResp ->
                            loop_active(UpdatedConn, UpdatedResp)
                    end;
                unknown ->
                    io:format("Unexpected message: ~p~n", [Message]),
                    error
            end
    end.

loop_passive(Conn, Resp) ->
    case ahttp_client:recv(Conn, 0) of
        {ok, UpdatedConn, Responses} ->
            case parse_responses(Responses, Resp, #{status => 200}) of
                #{done := true} ->
                    ahttp_client:close(Conn),
                    ok;
                UpdatedResp ->
                    loop_passive(UpdatedConn, UpdatedResp)
            end;
        Other ->
            io:format("Unexpected reply: ~p~n", [Other]),
            error
    end.

parse_responses([], Resp, _Expected) ->
    Resp;
parse_responses([{status, Ref, Code} | T], Resp, #{status := Code} = Expected) ->
    0 = map_size(Resp),
    parse_responses(T, #{ref => Ref, status => Code}, Expected);
parse_responses(
    [{header, Ref, {Name, Value}} | T], #{ref := Ref, status := _Status} = Resp, Expected
) when is_binary(Name) and is_binary(Value) ->
    parse_responses(T, Resp#{has_headers => true}, Expected);
parse_responses(
    [{data, Ref, Data} | T], #{ref := Ref, status := _Status, has_headers := true} = Resp, Expected
) when is_binary(Data) ->
    parse_responses(T, Resp#{has_data => true}, Expected);
parse_responses(
    [{done, Ref}],
    #{ref := Ref, status := _Status, has_headers := true, has_data := true} = Resp,
    _Expected
) ->
    Resp#{done => true}.

test_chunked_passive() ->
    Response = build_chunked_response([], [<<"Hello">>, <<"World">>], []),
    {ServerPid, Port} = start_chunked_server([Response]),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    Acc = loop_collect_passive(Conn2, #{}),
    #{status := 200, body := <<"HelloWorld">>, done := true} = Acc,
    wait_server(ServerPid),
    ok.

test_chunked_active() ->
    Response = build_chunked_response([], [<<"ping">>], []),
    {ServerPid, Port} = start_chunked_server([Response]),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, true}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    Acc = loop_collect_active(Conn2, #{}),
    #{status := 200, body := <<"ping">>, done := true} = Acc,
    wait_server(ServerPid),
    ok.

test_chunked_split_across_segments() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: chunked\r\n\r\n"
            "a\r\n12345"
        >>,
        <<"67890\r\n0\r\n\r\n">>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    Acc = loop_collect_passive(Conn2, #{}),
    #{status := 200, body := <<"1234567890">>, done := true} = Acc,
    wait_server(ServerPid),
    ok.

test_chunked_extension() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: chunked\r\n\r\n"
            "5;foo=bar\r\nHello\r\n"
            "0\r\n\r\n"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    Acc = loop_collect_passive(Conn2, #{}),
    #{status := 200, body := <<"Hello">>, done := true} = Acc,
    wait_server(ServerPid),
    ok.

test_chunked_trailer() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: chunked\r\n\r\n"
            "5\r\nHello\r\n"
            "0\r\nX-Digest: sha256-abc\r\n\r\n"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [
        {active, false}, {parse_headers, [<<"X-Digest">>]}
    ]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    Acc = loop_collect_passive(Conn2, #{}),
    #{
        status := 200,
        body := <<"Hello">>,
        done := true,
        trailers := [{<<"X-Digest">>, <<"sha256-abc">>}]
    } = Acc,
    wait_server(ServerPid),
    ok.

test_bad_transfer_encoding() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: gzip\r\n\r\n"
            "whatever"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    {error, {parser, {unsupported_transfer_encoding, <<"gzip">>}}} =
        ahttp_client:recv(Conn2, 0),
    ahttp_client:close(Conn2),
    wait_server(ServerPid),
    ok.

test_bad_transfer_encoding_stacked() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: gzip, chunked\r\n\r\n"
            "whatever"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    {error, {parser, {unsupported_transfer_encoding, <<"gzip, chunked">>}}} =
        ahttp_client:recv(Conn2, 0),
    ahttp_client:close(Conn2),
    wait_server(ServerPid),
    ok.

test_bad_transfer_encoding_bad_order() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: chunked, gzip\r\n\r\n"
            "whatever"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    {error, {parser, {unsupported_transfer_encoding, <<"chunked, gzip">>}}} =
        ahttp_client:recv(Conn2, 0),
    ahttp_client:close(Conn2),
    wait_server(ServerPid),
    ok.

test_content_length_and_transfer_encoding() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Content-Length: 5\r\n"
            "Transfer-Encoding: chunked\r\n\r\n"
            "5\r\nHello\r\n0\r\n\r\n"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    {error, {parser, {content_length_with_transfer_encoding, 5}}} =
        ahttp_client:recv(Conn2, 0),
    ahttp_client:close(Conn2),
    wait_server(ServerPid),
    ok.

test_transfer_encoding_before_content_length() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: chunked\r\n"
            "Content-Length: 5\r\n\r\n"
            "5\r\nHello\r\n0\r\n\r\n"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    {error, {parser, {content_length_with_transfer_encoding, 5}}} =
        ahttp_client:recv(Conn2, 0),
    ahttp_client:close(Conn2),
    wait_server(ServerPid),
    ok.

test_chunked_trailer_framing_filtered() ->
    Segments = [
        <<
            "HTTP/1.1 200 OK\r\n"
            "Transfer-Encoding: chunked\r\n\r\n"
            "5\r\nHello\r\n"
            "0\r\n"
            "Content-Length: 99999\r\n"
            "Transfer-Encoding: identity\r\n"
            "\r\n"
        >>
    ],
    {ServerPid, Port} = start_chunked_server(Segments),
    {ok, Conn} = ahttp_client:connect(http, "localhost", Port, [{active, false}]),
    {ok, Conn2, _Ref} = ahttp_client:request(Conn, <<"GET">>, <<"/">>, [], undefined),
    Acc = loop_collect_passive(Conn2, #{}),
    #{status := 200, body := <<"Hello">>, done := true} = Acc,
    [] = maps:get(trailers, Acc, []),
    wait_server(ServerPid),
    ok.

build_chunked_response(ExtraHeaders, Chunks, Trailers) ->
    HeaderLines = [[N, <<": ">>, V, <<"\r\n">>] || {N, V} <- ExtraHeaders],
    ChunkLines = [
        [integer_to_binary(byte_size(C), 16), <<"\r\n">>, C, <<"\r\n">>]
     || C <- Chunks
    ],
    TrailerLines = [[N, <<": ">>, V, <<"\r\n">>] || {N, V} <- Trailers],
    iolist_to_binary([
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Transfer-Encoding: chunked\r\n">>,
        HeaderLines,
        <<"\r\n">>,
        ChunkLines,
        <<"0\r\n">>,
        TrailerLines,
        <<"\r\n">>
    ]).

start_chunked_server(Segments) ->
    Parent = self(),
    Pid = spawn(fun() -> chunked_server_loop(Parent, Segments) end),
    receive
        {server_port, Pid, Port} -> {Pid, Port}
    after 5000 ->
        exit(Pid, kill),
        error(server_start_timeout)
    end.

chunked_server_loop(Parent, Segments) ->
    {ok, LSocket} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(LSocket),
    Parent ! {server_port, self(), Port},
    {ok, Socket} = gen_tcp:accept(LSocket, 5000),
    ok = drain_request_headers(Socket),
    send_segments(Socket, Segments),
    gen_tcp:close(Socket),
    gen_tcp:close(LSocket).

drain_request_headers(Socket) ->
    drain_request_headers(Socket, <<>>).

drain_request_headers(Socket, Acc) ->
    case gen_tcp:recv(Socket, 0, 2000) of
        {ok, Data} ->
            NewAcc = <<Acc/binary, Data/binary>>,
            case binary:match(NewAcc, <<"\r\n\r\n">>) of
                nomatch -> drain_request_headers(Socket, NewAcc);
                _ -> ok
            end;
        {error, _} ->
            ok
    end.

send_segments(_Socket, []) ->
    ok;
send_segments(Socket, [Segment | Rest]) ->
    ok = gen_tcp:send(Socket, Segment),
    case Rest of
        [] -> ok;
        _ -> receive
            after 50 -> ok
            end
    end,
    send_segments(Socket, Rest).

wait_server(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 ->
        demonitor(Ref, [flush]),
        exit(Pid, kill),
        error(server_did_not_exit)
    end.

loop_collect_passive(Conn, Acc) ->
    case ahttp_client:recv(Conn, 0) of
        {ok, UpdatedConn, Responses} ->
            NewAcc = accumulate(Responses, Acc),
            case maps:is_key(done, NewAcc) of
                true ->
                    ahttp_client:close(UpdatedConn),
                    NewAcc;
                false ->
                    loop_collect_passive(UpdatedConn, NewAcc)
            end
    end.

loop_collect_active(Conn, Acc) ->
    receive
        Msg ->
            case ahttp_client:stream(Conn, Msg) of
                {ok, _Conn, closed} ->
                    Acc;
                {ok, UpdatedConn, Responses} ->
                    NewAcc = accumulate(Responses, Acc),
                    case maps:is_key(done, NewAcc) of
                        true ->
                            ahttp_client:close(UpdatedConn),
                            NewAcc;
                        false ->
                            loop_collect_active(UpdatedConn, NewAcc)
                    end;
                unknown ->
                    loop_collect_active(Conn, Acc)
            end
    after 5000 ->
        error(no_response_timeout)
    end.

accumulate([], Acc) ->
    Acc;
accumulate([{status, _, Code} | T], Acc) ->
    accumulate(T, Acc#{status => Code});
accumulate([{header, _, _KV} | T], Acc) ->
    accumulate(T, Acc#{has_headers => true});
accumulate([{header_continuation, _, _KV} | T], Acc) ->
    accumulate(T, Acc);
accumulate([{trailer_header, _, KV} | T], Acc) ->
    Ts = maps:get(trailers, Acc, []),
    accumulate(T, Acc#{trailers => [KV | Ts]});
accumulate([{trailer_header_continuation, _, _KV} | T], Acc) ->
    accumulate(T, Acc);
accumulate([{data, _, Data} | T], Acc) ->
    Body = maps:get(body, Acc, <<>>),
    accumulate(T, Acc#{body => <<Body/binary, Data/binary>>});
accumulate([{done, _} | T], Acc) ->
    accumulate(T, Acc#{done => true}).
