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
%% @doc RTEMS platform helpers.
%%
%% Platform helpers packed into `atomvmlib-rtems`. UART, I2C, and GPIO live in
%% their respective Erlang modules with NIFs in the RTEMS C port. Networking
%% uses LibBSD on imx7 (`wait_dhcp/1`, `ifaddrs/0`) and returns
%% `{error, enotsup}` on erc32.
%% @end
%%-----------------------------------------------------------------------------
-module(atomvm_rtems).

-export([platform/0, wait_dhcp/1, ifaddrs/0]).

-define(DHCP_POLL_INTERVAL_MS, 250).

%% @doc Return the AtomVM platform atom for RTEMS.
-spec platform() -> rtems.
platform() ->
    atomvm:platform().

%% @doc Wait until `ffec0` has an IPv4 address, or until TimeoutMs elapses.
-spec wait_dhcp(TimeoutMs :: non_neg_integer()) -> ok | {error, timeout | enotsup | term()}.
wait_dhcp(TimeoutMs) when is_integer(TimeoutMs) andalso TimeoutMs >= 0 ->
    wait_dhcp_loop(TimeoutMs);
wait_dhcp(_TimeoutMs) ->
    erlang:error(badarg).

%% @doc Return IPv4 addresses as `{ok, [{IfName, {A,B,C,D}, Flags}]}`.
-spec ifaddrs() -> {ok, [{string(), {byte(), byte(), byte(), byte()}, integer()}]} | {error, term()}.
ifaddrs() ->
    erlang:nif_error(undefined).

%% @private
wait_dhcp_loop(RemainingMs) ->
    case ?MODULE:ifaddrs() of
        {ok, Addrs} ->
            case has_ipv4_address(Addrs, "ffec0") of
                true ->
                    ok;
                false when RemainingMs =:= 0 ->
                    {error, timeout};
                false ->
                    SleepMs = poll_interval(RemainingMs),
                    timer:sleep(SleepMs),
                    wait_dhcp_loop(RemainingMs - SleepMs)
            end;
        {error, eagain} when RemainingMs > 0 ->
            SleepMs = poll_interval(RemainingMs),
            timer:sleep(SleepMs),
            wait_dhcp_loop(RemainingMs - SleepMs);
        {error, eagain} ->
            {error, timeout};
        {error, _Reason} = Error ->
            Error
    end.

%% @private
has_ipv4_address([], _IfName) ->
    false;
has_ipv4_address([{IfName, {0, 0, 0, 0}, _Flags} | Rest], IfName) ->
    has_ipv4_address(Rest, IfName);
has_ipv4_address([{IfName, {_A, _B, _C, _D}, _Flags} | _Rest], IfName) ->
    true;
has_ipv4_address([_Other | Rest], IfName) ->
    has_ipv4_address(Rest, IfName).

%% @private
poll_interval(RemainingMs) when RemainingMs < ?DHCP_POLL_INTERVAL_MS ->
    RemainingMs;
poll_interval(_RemainingMs) ->
    ?DHCP_POLL_INTERVAL_MS.
