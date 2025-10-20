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
    case get_otp_version() of
        Version when Version =:= atomvm orelse (is_integer(Version) andalso Version >= 24) ->
            ok = test_passive_socket(),
            ok = test_active_socket();
        _ ->
            ok
    end.

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

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.
