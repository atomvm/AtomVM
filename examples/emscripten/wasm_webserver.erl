%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Toy web server that serve static files from src/platform/emscripten/build
%% This is used to demonstrate usage of wasm as a worker in the browser.
-module(wasm_webserver).

-export([start/0, handle_req/3]).

-spec start() -> no_return().
start() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    http_server:start_server(8080, Router),
    io:format("Open http://localhost:8080/\n"),
    receive
    after infinity -> ok
    end.

handle_req(Method, Path, Conn) when Method =:= "GET" orelse Method =:= "HEAD" ->
    Filename =
        case Path of
            [] ->
                "../examples/emscripten/index.html";
            ["AtomVM.js"] ->
                "../src/platforms/emscripten/build/src/AtomVM.js";
            ["AtomVM.wasm"] ->
                "../src/platforms/emscripten/build/src/AtomVM.wasm";
            ["AtomVM.worker.js"] ->
                "../src/platforms/emscripten/build/src/AtomVM.worker.js";
            ["tests", "build" | Tail] ->
                lists:flatten([
                    "../src/platforms/emscripten/build/tests/src/" | lists:join($/, Tail)
                ]);
            ["tests", "src" | Tail] ->
                lists:flatten(["../src/platforms/emscripten/tests/src/" | lists:join($/, Tail)]);
            ["build" | _] ->
                lists:flatten(["../" | lists:join($/, Path)]);
            _ ->
                lists:flatten(["../examples/emscripten/" | lists:join($/, Path)])
        end,
    MimeType =
        case lists:reverse(Filename) of
            "sj." ++ _ -> "text/javascript";
            "lmth." ++ _ -> "text/html";
            "msaw." ++ _ -> "application/wasm";
            "mva." ++ _ -> "application/binary";
            "maeb." ++ _ -> "application/binary";
            "pam." ++ _ -> "application/json";
            _ -> undefined
        end,
    case MimeType of
        undefined ->
            io:format("Unknown file suffix ~s\n", [Filename]),
            ErrorBody = <<"<html><body><h1>Forbidden</h1></body></html>">>,
            http_server:reply(403, ErrorBody, Conn);
        _ ->
            case atomvm:posix_open(Filename, [o_rdwr]) of
                {error, Err} ->
                    io:format("Could not open file ~s (~p)\n", [Filename, Err]),
                    ErrorBody = <<"<html><body><h1>Not Found</h1></body></html>">>,
                    http_server:reply(404, ErrorBody, Conn);
                {ok, Fd} ->
                    {Size, Data} = read_file(Fd, 0, []),
                    ok = atomvm:posix_close(Fd),
                    Headers = [
                        <<"Content-Type: ">>,
                        MimeType,
                        <<"\r\n">>,
                        <<"Content-Length: ">>,
                        integer_to_list(Size),
                        <<"\r\n">>,
                        <<"Connection: close\r\n">>,
                        <<"Server: AtomVM\r\n">>,
                        % The following are the required headers for wasm to work in worker
                        <<"Cross-Origin-Opener-Policy: same-origin\r\n">>,
                        <<"Cross-Origin-Embedder-Policy: require-corp\r\n">>
                    ],
                    io:format("~p 200 mime=~s size=~B\n", [Path, MimeType, Size]),
                    case Method of
                        "GET" -> http_server:reply(200, Data, Headers, Conn);
                        "HEAD" -> http_server:reply(200, <<>>, Headers, Conn)
                    end
            end
    end;
handle_req(Method, Path, Conn) ->
    io:format("Method: ~p Path: ~p~n", [Method, Path]),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).

read_file(Fd, Size, Acc) ->
    case atomvm:posix_read(Fd, 8192) of
        {ok, Data} ->
            ChunkSize = byte_size(Data),
            read_file(Fd, Size + ChunkSize, [Data | Acc]);
        eof ->
            {Size, lists:reverse(Acc)}
    end.
