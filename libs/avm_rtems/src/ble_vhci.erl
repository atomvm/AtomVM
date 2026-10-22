%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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
%% @doc Raw ESP32-C5 BLE controller transport.
%% Requires RTEMS_BT=ON and AVM_ENABLE_BLE_VHCI_NIF=ON in the firmware build.
%% A single process owns the handle. Incoming H4 packets arrive as
%% `{hci_event, Binary}' (including ACL packets), and send availability as
%% `{ble_vhci, ready_to_send}'. Queue overflow reports
%% `{ble_vhci, {error, overflow}}' and closes the session.
%% The caller implements HCI command credits and ACL flow control.
%% @end
%%-----------------------------------------------------------------------------
-module(ble_vhci).
-export([init/0, send/3, close/1]).
-export_type([handle/0]).

-opaque handle() :: {'$ble_vhci', reference()}.

%% @doc Start the controller without the advertising smoke test.
%% Waits for initialization while allowing other Erlang processes to run.
%% Owner exit or garbage collection of the handle requests shutdown.
-spec init() -> {ok, handle()} | {error, term()}.
init() -> erlang:nif_error(undefined).

%% @doc Send a complete H4 command or ACL packet. Owner must be self().
%% A successful send means that the controller transport accepted the packet;
%% the caller must still wait for HCI command completion/status and credits.
-spec send(handle(), pid(), binary()) -> ok | {error, term()}.
send(_Handle, _Owner, _Packet) -> erlang:nif_error(undefined).

%% @doc Stop the controller. Closing an already closed handle is harmless.
%% Waits for shutdown while allowing other Erlang processes to run.
-spec close(handle()) -> ok | {error, term()}.
close(_Handle) -> erlang:nif_error(undefined).
