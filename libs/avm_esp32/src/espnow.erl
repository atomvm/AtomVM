%
% This file is part of AtomVM.
%
% Copyright 2026 Gabriel Mancini <gabriel.mancini@gmail.com>
%
% The API follows harmon25/atomvm_espnow (Apache-2.0).
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
%% @doc ESP-NOW: connectionless frames between ESP32 devices, on the Wi-Fi
%% radio, with no access point: up to 1470 bytes on ESP-IDF 5.4 or newer
%% (ESP-NOW v2, which peers on an older ESP-IDF cannot receive), 250 before.
%%
%% Needs `CONFIG_AVM_ENABLE_ESPNOW'. The network driver owns the radio: ESP-NOW
%% works with or without a network, and survives `network:stop/0' and
%% `network:start/1'. With the STA connected, frames go on the access point's
%% channel; otherwise on the channel given to `open/1' or `set_channel/2'.
%%
%% The receiver (the caller of `open/1' unless `{receiver, Pid}') gets
%% `{espnow, rx, Mac, Data, #{rssi := Rssi}}' for each frame. `send/3'
%% returns once the frame is out.
%% @end
%%-----------------------------------------------------------------------------
-module(espnow).

-export([
    open/0, open/1,
    close/1,
    add_peer/2, add_peer/3,
    del_peer/2,
    send/3,
    set_channel/2,
    get_channel/1,
    stats/1
]).

-type mac() :: <<_:48>>.
%% A peer's station MAC address.
-type channel() :: 0..14.
%% A Wi-Fi channel; 0 is the current one.
-type key() :: <<_:128>>.

-type open_option() ::
    {receiver, pid()}
    | {channel, channel()}
    | {pmk, key()}.
%% `pmk' is the primary key that encrypts the peers' `lmk' keys.

-type peer_option() :: {channel, channel()} | {lmk, key()}.
%% `lmk' encrypts the traffic with this peer (at most 6 encrypted peers).

-export_type([mac/0, channel/0]).

%% @equiv open([])
-spec open() -> {ok, port()} | {error, term()}.
open() ->
    open([]).

%% @doc Starts ESP-NOW, turning the Wi-Fi radio on if no network has it.
%% Only one port can be open: `{error, already_open}' while another is. An
%% AP-only network cannot carry ESP-NOW (its frames go out on the STA):
%% `{error, ap_only}'. `{error, badarg}' is a bad option. The port is linked
%% to the caller: if the caller dies the port closes, and ESP-NOW can be
%% opened again.
-spec open([open_option()]) -> {ok, port()} | {error, term()}.
open(Options) ->
    Receiver = proplists:get_value(receiver, Options, self()),
    try open_port({spawn, "espnow"}, []) of
        Port ->
            Opts = [{receiver, Receiver} | proplists:delete(receiver, Options)],
            case port:call(Port, {open, Opts}) of
                ok ->
                    % The caller owns the port: if it dies, the port goes with
                    % it, and the next open/1 can open ESP-NOW again.
                    link(Port),
                    {ok, Port};
                {error, _} = Error ->
                    Error
            end
    catch
        error:Reason -> {error, Reason}
    end.

%% @doc Stops ESP-NOW; the radio goes off if no network uses it.
-spec close(port()) -> ok.
close(Port) ->
    port:call(Port, close).

%% @equiv add_peer(Port, Mac, [])
-spec add_peer(port(), mac()) -> ok | {error, term()}.
add_peer(Port, Mac) ->
    add_peer(Port, Mac, []).

%% @doc Adds a unicast peer. Broadcast needs no peer: `broadcast' is
%% `badarg' here and in `del_peer/2'. ESP-NOW errors come as
%% `{error, not_found | peer_exists | peer_list_full | interface_mismatch |
%% channel_mismatch | no_mem}', others as the ESP-IDF error.
-spec add_peer(port(), mac(), [peer_option()]) -> ok | {error, term()}.
add_peer(Port, Mac, Options) ->
    port:call(Port, {add_peer, Mac, Options}).

-spec del_peer(port(), mac()) -> ok | {error, term()}.
del_peer(Port, Mac) ->
    port:call(Port, {del_peer, Mac}).

%% @doc Sends `Data' (1 to 1470 bytes on ESP-IDF 5.4 or newer, 250 before)
%% to `Mac', a peer, or `broadcast', and waits for the result: `ok' (for a
%% unicast frame the peer acknowledged it, for a broadcast it went out) or
%% `{error, fail}'. The driver gives the radio one frame at a time, the next
%% once the previous one has its result, as the ESP-NOW guide recommends;
%% concurrent callers wait their turn. `{error, closed}' if the port closes
%% first.
-spec send(port(), mac() | broadcast, binary()) -> ok | {error, term()}.
send(Port, Mac, Data) ->
    port:call(Port, {send, Mac, Data}).

%% @doc Moves to `Channel' (1 to 14). Refused with `{error, sta_connected}'
%% while the STA is associated: its channel is the access point's, and moving
%% the radio would take the STA off it.
-spec set_channel(port(), 1..14) -> ok | {error, term()}.
set_channel(Port, Channel) ->
    port:call(Port, {set_channel, Channel}).

-spec get_channel(port()) -> {ok, 1..14} | {error, term()}.
get_channel(Port) ->
    port:call(Port, get_channel).

%% @doc `dropped': received frames lost before reaching the receiver: the
%% frames waiting between the Wi-Fi task and the scheduler already held
%% `CONFIG_AVM_ESPNOW_RX_BUDGET' bytes (8192 by default), their queue was
%% full, or there was no memory. The receiver's mailbox is not bounded.
-spec stats(port()) -> {ok, #{dropped := non_neg_integer()}}.
stats(Port) ->
    port:call(Port, stats).
