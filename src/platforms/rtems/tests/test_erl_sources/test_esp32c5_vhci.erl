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

-module(test_esp32c5_vhci).
-export([start/0]).

start() ->
    erlang:display({esp32c5_vhci, start}),
    {ok, Handle} = ble_vhci:init(),
    {error, already_started} = ble_vhci:init(),
    {error, eio} = atomvm_rtems:bt_start_async(),
    command(Handle, 16#0c03, <<>>),
    command(Handle, 16#1001, <<>>),
    command(Handle, 16#1009, <<>>),
    command(Handle, 16#2002, <<>>),
    lists:foreach(fun(Packet) ->
        badarg(fun() -> ble_vhci:send(Handle, self(), Packet) end)
    end, [<<>>, <<1>>, <<1, 3, 12>>, <<1, 3, 12, 1>>, <<1, 3, 12, 0, 0>>,
        <<4, 14, 0, 0>>, <<2, 0, 0, 1, 0>>, <<2, 0, 0, 6, 2, 0:4144>>]),
    Parent = self(),
    spawn(fun() ->
        ok = badarg(fun() -> ble_vhci:send(Handle, Parent, <<1, 3, 12, 0>>) end),
        ok = badarg(fun() -> ble_vhci:close(Handle) end),
        Parent ! foreign_owner_checked
    end),
    receive foreign_owner_checked -> ok after 5000 -> erlang:error(foreign_owner_timeout) end,
    ok = ble_vhci:close(Handle),
    ok = ble_vhci:close(Handle),
    ok = badarg(fun() -> ble_vhci:send(Handle, self(), <<1, 3, 12, 0>>) end),
    {ok, Second} = ble_vhci:init(),
    ok = ble_vhci:close(Handle),
    command(Second, 16#0c03, <<>>),
    ok = ble_vhci:close(Second),
    erlang:display({vhci_close_reopen, ok}),
    Orphan = owner_exit(Parent),
    {ok, Third} = reopen(100),
    {'$ble_vhci', _} = Orphan,
    command(Third, 16#0c03, <<>>),
    ok = ble_vhci:close(Third),
    erlang:display({vhci_owner_exit, ok}),
    GcOwner = spawn(fun() ->
        gc_session(),
        erlang:garbage_collect(),
        Parent ! gc_done,
        receive stop -> ok end
    end),
    receive gc_done -> ok after 5000 -> erlang:error(gc_timeout) end,
    {ok, Fourth} = reopen(100),
    command(Fourth, 16#0c03, <<>>),
    GcOwner ! stop,
    ok = ble_vhci:close(Fourth),
    erlang:display({vhci_gc_cleanup, ok}),
    % Leave one session to verify that VM teardown joins the worker.
    {ok, Last} = ble_vhci:init(),
    command(Last, 16#0c03, <<>>),
    erlang:display({vhci_test_complete, ok}),
    ok.

badarg(Fun) ->
    try Fun() of
        Result -> erlang:error({expected_badarg, Result})
    catch error:badarg -> ok end.

command(Handle, Opcode, Params) ->
    ok = ble_vhci:send(Handle, self(), <<1, Opcode:16/little, (byte_size(Params)), Params/binary>>),
    receive
        {hci_event, <<4, 14, _Length, _Credits, Opcode:16/little, 0, _/binary>>} -> ok
    after 5000 -> erlang:error({hci_timeout, Opcode}) end.

owner_exit(Parent) ->
    spawn(fun() ->
        {ok, Handle} = ble_vhci:init(),
        command(Handle, 16#0c03, <<>>),
        Parent ! {owner_ready, Handle}
    end),
    receive {owner_ready, Handle} -> Handle after 5000 -> erlang:error(owner_timeout) end.

gc_session() ->
    {ok, Handle} = ble_vhci:init(),
    command(Handle, 16#0c03, <<>>).

reopen(0) -> erlang:error(reopen_timeout);
reopen(N) ->
    case ble_vhci:init() of
        {error, already_started} -> timer:sleep(10), reopen(N - 1);
        Result -> Result
    end.
