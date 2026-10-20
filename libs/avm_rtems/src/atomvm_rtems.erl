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
%% uses LibBSD on imx7 (`wait_dhcp/1`, `wait_resolver/1`, `ifaddrs/0`) and
%% returns `{error, enotsup}` on BSPs without LibBSD.
%% @end
%%-----------------------------------------------------------------------------
-module(atomvm_rtems).

-export([
    platform/0,
    wait_dhcp/1,
    wait_dhcp/2,
    wait_wifi/1,
    wait_resolver/1,
    ifaddrs/0,
    resolver_ready/0,
    wifi_start/2,
    wifi_start/3,
    wifi_stop/0,
    wifi_status/0,
    bt_start/0,
    bt_start_async/0,
    bt_status/0,
    rtc_slow_get_binary/0,
    rtc_slow_set_binary/1,
    flash_get_size/0,
    flash_read/2,
    flash_write/2,
    flash_erase_sector/1,
    flash_erase_range/2,
    nvs_fetch_binary/2,
    nvs_put_binary/3,
    nvs_erase_key/2,
    nvs_erase_all/1,
    nvs_reformat/0,
    flash_fs_mount/0,
    flash_fs_mount/1,
    flash_fs_unmount/0,
    flash_fs_unmount/1,
    flash_fs_is_mounted/0,
    read_file/1,
    write_file/2
]).

-define(DHCP_POLL_INTERVAL_MS, 250).

%% @doc Return the AtomVM platform atom for RTEMS.
-spec platform() -> rtems.
platform() ->
    atomvm:platform().

%% @doc Wait until `ffec0` or `wlan0` has an IPv4 address, or until TimeoutMs elapses.
-spec wait_dhcp(TimeoutMs :: non_neg_integer()) -> ok | {error, timeout | enotsup | term()}.
wait_dhcp(TimeoutMs) when is_integer(TimeoutMs) andalso TimeoutMs >= 0 ->
    wait_dhcp_any_loop(TimeoutMs);
wait_dhcp(_TimeoutMs) ->
    erlang:error(badarg).

%% @doc Wait until specified interface (e.g. `"ffec0"` or `"wlan0"`) has an IPv4 address.
-spec wait_dhcp(IfName :: string() | binary(), TimeoutMs :: non_neg_integer()) ->
    ok | {error, timeout | enotsup | term()}.
wait_dhcp(IfName, TimeoutMs) when is_binary(IfName) ->
    wait_dhcp(binary_to_list(IfName), TimeoutMs);
wait_dhcp(IfName, TimeoutMs) when
    is_list(IfName) andalso is_integer(TimeoutMs) andalso TimeoutMs >= 0
->
    wait_dhcp_loop(IfName, TimeoutMs);
wait_dhcp(_IfName, _TimeoutMs) ->
    erlang:error(badarg).

%% @doc Wait until `wlan0` has associated and acquired an IPv4 address.
-spec wait_wifi(TimeoutMs :: non_neg_integer()) -> ok | {error, timeout | enotsup | term()}.
wait_wifi(TimeoutMs) ->
    wait_dhcp("wlan0", TimeoutMs).

%% @doc Wait until dhcpcd has atomically installed resolver configuration.
-spec wait_resolver(TimeoutMs :: non_neg_integer()) -> ok | {error, timeout | enotsup | term()}.
wait_resolver(TimeoutMs) when is_integer(TimeoutMs) andalso TimeoutMs >= 0 ->
    wait_resolver_loop(TimeoutMs);
wait_resolver(_TimeoutMs) ->
    erlang:error(badarg).

%% @doc Return IPv4 addresses as `{ok, [{IfName, {A,B,C,D}, Flags}]}`.
-spec ifaddrs() ->
    {ok, [{string(), {byte(), byte(), byte(), byte()}, integer()}]} | {error, term()}.
ifaddrs() ->
    erlang:nif_error(undefined).

%% @doc Return whether dhcpcd has installed resolver configuration.
-spec resolver_ready() -> boolean() | {error, enotsup}.
resolver_ready() ->
    erlang:nif_error(undefined).

%% @doc Start Wi-Fi interface and associate to an SSID with PSK (or `none`).
-spec wifi_start(SSID :: string() | binary(), PSK :: string() | binary() | none) ->
    ok | {error, term()}.
wifi_start(_SSID, _PSK) ->
    erlang:nif_error(undefined).

%% @doc Start Wi-Fi, optionally leaving the station unassociated.
-spec wifi_start(string() | binary(), string() | binary() | none, boolean()) ->
    ok | {error, term()}.
wifi_start(_SSID, _PSK, _Connect) ->
    erlang:nif_error(undefined).

%% @doc Start the minimal non-connectable BLE advertiser, yielding while it initializes.
-spec bt_start() -> ok | {error, term()}.
bt_start() ->
    case ?MODULE:bt_start_async() of
        ok -> bt_wait_started(2000);
        Error -> Error
    end.

%% @doc Start BLE initialization in a native task; inspect completion with bt_status/0.
-spec bt_start_async() -> ok | {error, term()}.
bt_start_async() ->
    erlang:nif_error(undefined).

%% @doc Return BLE initialization state, including HCI advertising completion.
-spec bt_status() -> stopped | starting | started | failed | {error, term()}.
bt_status() ->
    erlang:nif_error(undefined).

bt_wait_started(0) ->
    {error, timeout};
bt_wait_started(Attempts) ->
    case ?MODULE:bt_status() of
        started -> ok;
        starting -> timer:sleep(10), bt_wait_started(Attempts - 1);
        failed -> {error, eio};
        Error -> Error
    end.

%% @doc Stop Wi-Fi interface and bring down `wlan0`.
-spec wifi_stop() -> ok | {error, term()}.
wifi_stop() ->
    erlang:nif_error(undefined).

%% @doc Return current Wi-Fi status and IP parameters.
-spec wifi_status() -> {ok, [{atom(), term()}]} | {error, term()}.
wifi_status() ->
    erlang:nif_error(undefined).

%% @doc Return binary stored in RTC slow/fast memory (ESP32-C3).
-spec rtc_slow_get_binary() -> binary().
rtc_slow_get_binary() ->
    erlang:nif_error(undefined).

%% @doc Store binary in RTC slow/fast memory (ESP32-C3).
-spec rtc_slow_set_binary(Bin :: binary()) -> ok.
rtc_slow_set_binary(_Bin) ->
    erlang:nif_error(undefined).

%% @doc Return flash chip size in bytes.
-spec flash_get_size() -> non_neg_integer().
flash_get_size() ->
    erlang:nif_error(undefined).

%% @doc Read Length bytes from flash at Offset.
-spec flash_read(Offset :: non_neg_integer(), Length :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
flash_read(_Offset, _Length) ->
    erlang:nif_error(undefined).

%% @doc Write Binary to flash at Offset.
-spec flash_write(Offset :: non_neg_integer(), Data :: binary()) -> ok | {error, term()}.
flash_write(_Offset, _Data) ->
    erlang:nif_error(undefined).

%% @doc Erase a 4096-byte sector of flash.
-spec flash_erase_sector(SectorNum :: non_neg_integer()) -> ok | {error, term()}.
flash_erase_sector(_SectorNum) ->
    erlang:nif_error(undefined).

%% @doc Erase a range of flash sectors.
-spec flash_erase_range(Offset :: non_neg_integer(), Length :: non_neg_integer()) ->
    ok | {error, term()}.
flash_erase_range(_Offset, _Length) ->
    erlang:nif_error(undefined).

%% @doc Fetch binary stored at Namespace and Key in NVS.
-spec nvs_fetch_binary(Namespace :: atom(), Key :: atom()) ->
    {ok, binary()} | {error, not_found | namespace_not_found | term()}.
nvs_fetch_binary(_Namespace, _Key) ->
    erlang:nif_error(undefined).

%% @doc Store binary in NVS key-value storage.
-spec nvs_put_binary(Namespace :: atom(), Key :: atom(), Value :: binary()) -> ok.
nvs_put_binary(_Namespace, _Key, _Value) ->
    erlang:nif_error(undefined).

%% @doc Erase key from specified namespace in NVS.
-spec nvs_erase_key(Namespace :: atom(), Key :: atom()) -> ok.
nvs_erase_key(_Namespace, _Key) ->
    erlang:nif_error(undefined).

%% @doc Erase all keys in specified namespace in NVS.
-spec nvs_erase_all(Namespace :: atom()) -> ok.
nvs_erase_all(_Namespace) ->
    erlang:nif_error(undefined).

%% @doc Reformat NVS storage partition.
-spec nvs_reformat() -> ok.
nvs_reformat() ->
    erlang:nif_error(undefined).

%% @doc Mount JFFS2 filesystem on `/flash`.
-spec flash_fs_mount() -> ok | {error, term()}.
flash_fs_mount() ->
    erlang:nif_error(undefined).

%% @doc Mount JFFS2 filesystem on specified mount point.
-spec flash_fs_mount(MountPoint :: string() | binary()) -> ok | {error, term()}.
flash_fs_mount(_MountPoint) ->
    erlang:nif_error(undefined).

%% @doc Unmount JFFS2 filesystem from `/flash`.
-spec flash_fs_unmount() -> ok | {error, term()}.
flash_fs_unmount() ->
    erlang:nif_error(undefined).

%% @doc Unmount JFFS2 filesystem from specified mount point.
-spec flash_fs_unmount(MountPoint :: string() | binary()) -> ok | {error, term()}.
flash_fs_unmount(_MountPoint) ->
    erlang:nif_error(undefined).

%% @doc Return whether flash filesystem is currently mounted.
-spec flash_fs_is_mounted() -> boolean().
flash_fs_is_mounted() ->
    erlang:nif_error(undefined).

%% @doc Read entire file into binary.
-spec read_file(Path :: string() | binary()) -> {ok, binary()} | {error, term()}.
read_file(_Path) ->
    erlang:nif_error(undefined).

%% @doc Write binary data to file.
-spec write_file(Path :: string() | binary(), Data :: binary()) -> ok | {error, term()}.
write_file(_Path, _Data) ->
    erlang:nif_error(undefined).

%% @private
wait_dhcp_loop(IfName, RemainingMs) ->
    case ?MODULE:ifaddrs() of
        {ok, Addrs} ->
            case has_ipv4_address(Addrs, IfName) of
                true ->
                    ok;
                false when RemainingMs =:= 0 ->
                    {error, timeout};
                false ->
                    SleepMs = poll_interval(RemainingMs),
                    timer:sleep(SleepMs),
                    wait_dhcp_loop(IfName, RemainingMs - SleepMs)
            end;
        {error, eagain} when RemainingMs > 0 ->
            SleepMs = poll_interval(RemainingMs),
            timer:sleep(SleepMs),
            wait_dhcp_loop(IfName, RemainingMs - SleepMs);
        {error, eagain} ->
            {error, timeout};
        {error, _Reason} = Error ->
            Error
    end.

%% @private
wait_dhcp_any_loop(RemainingMs) ->
    case ?MODULE:ifaddrs() of
        {ok, Addrs} ->
            case has_ipv4_address(Addrs, "ffec0") orelse has_ipv4_address(Addrs, "wlan0") of
                true ->
                    ok;
                false when RemainingMs =:= 0 ->
                    {error, timeout};
                false ->
                    SleepMs = poll_interval(RemainingMs),
                    timer:sleep(SleepMs),
                    wait_dhcp_any_loop(RemainingMs - SleepMs)
            end;
        {error, eagain} when RemainingMs > 0 ->
            SleepMs = poll_interval(RemainingMs),
            timer:sleep(SleepMs),
            wait_dhcp_any_loop(RemainingMs - SleepMs);
        {error, eagain} ->
            {error, timeout};
        {error, _Reason} = Error ->
            Error
    end.

%% @private
wait_resolver_loop(RemainingMs) ->
    case ?MODULE:resolver_ready() of
        true ->
            ok;
        false when RemainingMs =:= 0 ->
            {error, timeout};
        false ->
            SleepMs = poll_interval(RemainingMs),
            timer:sleep(SleepMs),
            wait_resolver_loop(RemainingMs - SleepMs);
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
