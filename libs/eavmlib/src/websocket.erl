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

%%-----------------------------------------------------------------------------
%% @doc websocket API
%%
%% The functions in this module are currently only implemented for the
%% emscripten platform.
%%
%% @end
%%-----------------------------------------------------------------------------
-module(websocket).
-export([
    is_supported/0,
    new/1,
    new/2,
    new/3,
    controlling_process/2,
    ready_state/1,
    buffered_amount/1,
    url/1,
    extensions/1,
    protocol/1,
    send_utf8/2,
    send_binary/2,
    close/1,
    close/2,
    close/3
]).

-export_type([
    websocket/0,
    ready_state/0
]).

-opaque websocket() :: {'$websocket', reference(), binary()}.
-type ready_state() :: connecting | open | closing | closed.

-define(CLOSE_STATUS_CODE_NORMAL, 1000).

%%-----------------------------------------------------------------------------
%% @return `true' if websockets are supported
%% @doc Determine if websockets are supported in the current environment
%% (browser).
%% @end
%%-----------------------------------------------------------------------------
-spec is_supported() -> boolean().
is_supported() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @equiv new(URL, <<"binary,base64">>, self())
%% @doc Open a new websocket with the given server.
%% @end
%%-----------------------------------------------------------------------------
-spec new(iodata()) -> websocket().
new(URL) ->
    ?MODULE:new(URL, []).

%%-----------------------------------------------------------------------------
%% @equiv new(URL, Protocols, self())
%% @doc Open a new websocket with the given server.
%% @end
%%-----------------------------------------------------------------------------
-spec new(iodata(), [iodata()]) -> websocket().
new(URL, Protocols) ->
    ?MODULE:new(URL, Protocols, self()).

%%-----------------------------------------------------------------------------
%% @param URL URL to connect to
%% @param Protocols List of protocol strings for negociation
%% with the server.
%% @param ControllingProcess Process that gets messages from the websocket
%% @return a websocket resource
%% @doc Open a new websocket with the given server.
%% @end
%%-----------------------------------------------------------------------------
-spec new(iodata(), [iodata()], pid()) -> websocket().
new(_URL, _Protocols, _ControllingProcess) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to set the controlling process for
%% @param ControllingProcess Process that gets messages from the websocket
%% @return `ok' or an error tuple
%% @doc Set the controlling process of the websocket. Can only be called from
%% the current owner of the websocket.
%% @end
%%-----------------------------------------------------------------------------
-spec controlling_process(websocket(), pid()) -> ok | {error, not_owner | badarg | closed}.
controlling_process(_Websocket, _ControllingProcess) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to get the ready state of
%% @return the ready state of the socket
%% @doc Returns the state of the websocket connection.
%% @end
%%-----------------------------------------------------------------------------
-spec ready_state(websocket()) -> ready_state().
ready_state(_Websocket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to get the buffered amount of
%% @return a number of bytes
%% @doc Returns the number of bytes of application data that have been queued
%% using `send/2' but not yet been transmitted to the network.
%% @end
%%-----------------------------------------------------------------------------
-spec buffered_amount(websocket()) -> non_neg_integer().
buffered_amount(_Websocket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to get the URL of
%% @return the websocket URL
%% @doc Returns the URL that was used to open the websocket
%% @end
%%-----------------------------------------------------------------------------
-spec url(websocket()) -> unicode:chardata().
url(_Websocket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to get the extensions of
%% @return the websocket extensions
%% @doc Returns the extensions selected by the server, if any.
%% @end
%%-----------------------------------------------------------------------------
-spec extensions(websocket()) -> unicode:chardata().
extensions(_Websocket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to get the protocol of
%% @return the websocket protocol
%% @doc Returns the subprotocol selected by the server, if any.
%% @end
%%-----------------------------------------------------------------------------
-spec protocol(websocket()) -> unicode:chardata().
protocol(_Websocket) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to send data to
%% @param String string to send
%% @return `ok' or an error tuple.
%% @doc Send data to the websocket.
%% @end
%%-----------------------------------------------------------------------------
-spec send_utf8(WebSocket :: websocket(), String :: iodata()) -> ok | {error, closed}.
send_utf8(_Websocket, _String) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to send data to
%% @param Data data to send
%% @return `ok' or an error tuple.
%% @doc Send data to the websocket.
%% @end
%%-----------------------------------------------------------------------------
-spec send_binary(websocket(), iodata()) -> ok | {error, closed}.
send_binary(_Websocket, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @equiv close(Websocket, CLOSE_STATUS_CODE_NORMAL, [])
%% @param Websocket websocket to close
%% @return `ok'
%% @doc Close websocket
%% @end
%%-----------------------------------------------------------------------------
-spec close(websocket()) -> ok.
close(Websocket) ->
    ?MODULE:close(Websocket, ?CLOSE_STATUS_CODE_NORMAL).

%%-----------------------------------------------------------------------------
%% @equiv close(Websocket, StatusCode, [])
%% @param Websocket websocket to close
%% @param StatusCode status code to send to server
%% @return `ok'
%% @doc Close websocket
%% @end
%%-----------------------------------------------------------------------------
-spec close(websocket(), integer()) -> ok.
close(Websocket, StatusCode) ->
    ?MODULE:close(Websocket, StatusCode, []).

%%-----------------------------------------------------------------------------
%% @param Websocket websocket to close
%% @param StatusCode status code to send to server
%% @param Reason reason to send to server
%% @return `ok'
%% @doc Close websocket
%% @end
%%-----------------------------------------------------------------------------
-spec close(websocket(), integer(), iodata()) -> ok.
close(_Websocket, _StatusCode, _Reason) ->
    erlang:nif_error(undefined).
