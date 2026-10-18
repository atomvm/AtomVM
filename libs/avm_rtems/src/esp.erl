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
%% @doc ESP32 / ESP32-C3 APIs under RTEMS
%%
%% This module implements the standard AtomVM `esp` API for ESP32 hardware
%% running under RTEMS, including NVS key-value storage, SPI flash access,
%% hardware RTC slow memory, reset reason, sleep modes, hardware timers, and
%% Task Watchdog Timers.
%% @end
%%-----------------------------------------------------------------------------
-module(esp).

-export([
    reset_reason/0,
    timer_get_time/0,
    rtc_get_time_us/0,
    sleep_get_wakeup_cause/0,
    sleep_enable_timer_wakeup/1,
    sleep_enable_gpio_wakeup/0,
    deep_sleep_enable_gpio_wakeup/2,
    deep_sleep_hold_en/0,
    deep_sleep_hold_dis/0,
    deep_sleep/0,
    deep_sleep/1,
    light_sleep/0,
    rtc_slow_get_binary/0,
    rtc_slow_set_binary/1,
    task_wdt_init/1,
    task_wdt_reconfigure/1,
    task_wdt_deinit/0,
    task_wdt_add_user/1,
    task_wdt_reset_user/1,
    task_wdt_delete_user/1,
    wdt_feed/0,
    nvs_fetch_binary/2,
    nvs_get_binary/1, nvs_get_binary/2, nvs_get_binary/3,
    nvs_set_binary/2, nvs_set_binary/3,
    nvs_put_binary/3,
    nvs_erase_key/1, nvs_erase_key/2,
    nvs_erase_all/0, nvs_erase_all/1,
    nvs_reformat/0,
    flash_get_size/0,
    flash_read/2,
    flash_write/2,
    flash_erase_sector/1,
    flash_erase_range/2,
    flash_fs_mount/0, flash_fs_mount/1,
    flash_fs_unmount/0, flash_fs_unmount/1,
    flash_fs_is_mounted/0
]).

-define(ATOMVM_NVS_NS, atomvm).

%% @doc Return the reset reason of the CPU.
-spec reset_reason() ->
    esp_rst_unknown
    | esp_rst_poweron
    | esp_rst_ext
    | esp_rst_sw
    | esp_rst_panic
    | esp_rst_int_wdt
    | esp_rst_task_wdt
    | esp_rst_wdt
    | esp_rst_deepsleep
    | esp_rst_brownout
    | esp_rst_sdio.
reset_reason() ->
    erlang:nif_error(undefined).

%% @doc Return the microsecond monotonic time since startup.
-spec timer_get_time() -> non_neg_integer().
timer_get_time() ->
    erlang:nif_error(undefined).

%% @doc Return microsecond time from continuous hardware RTC counter.
-spec rtc_get_time_us() -> non_neg_integer().
rtc_get_time_us() ->
    erlang:nif_error(undefined).

%% @doc Return wakeup cause after sleep.
-spec sleep_get_wakeup_cause() ->
    undefined
    | sleep_wakeup_ext0
    | sleep_wakeup_ext1
    | sleep_wakeup_timer
    | sleep_wakeup_touchpad
    | sleep_wakeup_ulp
    | sleep_wakeup_gpio
    | sleep_wakeup_uart
    | sleep_wakeup_wifi
    | sleep_wakeup_cocpu.
sleep_get_wakeup_cause() ->
    erlang:nif_error(undefined).

%% @doc Configure timer wakeup duration in microseconds.
-spec sleep_enable_timer_wakeup(TimeInMicroseconds :: non_neg_integer()) -> ok.
sleep_enable_timer_wakeup(_TimeInMicroseconds) ->
    erlang:nif_error(undefined).

%% @doc Enable GPIO wakeup for light sleep.
-spec sleep_enable_gpio_wakeup() -> ok.
sleep_enable_gpio_wakeup() ->
    erlang:nif_error(undefined).

%% @doc Enable GPIO wakeup for deep sleep with pin mask and mode.
-spec deep_sleep_enable_gpio_wakeup(Mask :: non_neg_integer(), Mode :: 0 | 1) -> ok.
deep_sleep_enable_gpio_wakeup(_Mask, _Mode) ->
    erlang:nif_error(undefined).

%% @doc Freeze GPIO pad output and pull states during deep sleep.
-spec deep_sleep_hold_en() -> ok.
deep_sleep_hold_en() ->
    erlang:nif_error(undefined).

%% @doc Unfreeze GPIO pad states after deep sleep wakeup.
-spec deep_sleep_hold_dis() -> ok.
deep_sleep_hold_dis() ->
    erlang:nif_error(undefined).

%% @doc Enter deep sleep indefinitely.
-spec deep_sleep() -> no_return().
deep_sleep() ->
    erlang:nif_error(undefined).

%% @doc Enter deep sleep for specified microseconds.
-spec deep_sleep(TimeInMicroseconds :: non_neg_integer()) -> no_return().
deep_sleep(_TimeInMicroseconds) ->
    erlang:nif_error(undefined).

%% @doc Enter light sleep. Execution resumes after wakeup event.
-spec light_sleep() -> ok.
light_sleep() ->
    erlang:nif_error(undefined).

%% @doc Read binary stored in RTC slow memory (persists across deep sleep).
-spec rtc_slow_get_binary() -> binary().
rtc_slow_get_binary() ->
    erlang:nif_error(undefined).

%% @doc Store binary in RTC slow memory.
-spec rtc_slow_set_binary(Bin :: binary()) -> ok.
rtc_slow_set_binary(_Bin) ->
    erlang:nif_error(undefined).

%% @doc Initialize Task Watchdog Timer.
-spec task_wdt_init(
    Config ::
        #{
            timeout_ms := pos_integer(),
            idle_core_mask => non_neg_integer(),
            trigger_panic => boolean()
        }
        | pos_integer()
) -> ok.
task_wdt_init(_Config) ->
    erlang:nif_error(undefined).

%% @doc Reconfigure Task Watchdog Timer parameters.
-spec task_wdt_reconfigure(
    Config :: #{
        timeout_ms := pos_integer(), idle_core_mask => non_neg_integer(), trigger_panic => boolean()
    }
) -> ok.
task_wdt_reconfigure(_Config) ->
    erlang:nif_error(undefined).

%% @doc Deinitialize Task Watchdog Timer.
-spec task_wdt_deinit() -> ok.
task_wdt_deinit() ->
    erlang:nif_error(undefined).

%% @doc Register user task name with TWDT.
-spec task_wdt_add_user(UserName :: binary() | string()) -> {ok, reference()}.
task_wdt_add_user(_UserName) ->
    erlang:nif_error(undefined).

%% @doc Feed watchdog for registered user task.
-spec task_wdt_reset_user(UserRef :: reference()) -> ok.
task_wdt_reset_user(_UserRef) ->
    erlang:nif_error(undefined).

%% @doc Unregister user task from TWDT.
-spec task_wdt_delete_user(UserRef :: reference()) -> ok.
task_wdt_delete_user(_UserRef) ->
    erlang:nif_error(undefined).

%% @doc Feed hardware and software watchdogs.
-spec wdt_feed() -> ok.
wdt_feed() ->
    erlang:nif_error(undefined).

%%
%% NVS (Non-Volatile Storage) APIs
%%

%% @doc Fetch binary stored at Namespace and Key.
-spec nvs_fetch_binary(Namespace :: atom(), Key :: atom()) ->
    {ok, binary()} | {error, not_found | namespace_not_found | term()}.
nvs_fetch_binary(Namespace, Key) when is_atom(Namespace) andalso is_atom(Key) ->
    erlang:nif_error(undefined).

%% @doc Equivalent to `nvs_get_binary(atomvm, Key)`.
-spec nvs_get_binary(Key :: atom()) -> binary() | undefined.
nvs_get_binary(Key) when is_atom(Key) ->
    esp:nvs_get_binary(?ATOMVM_NVS_NS, Key).

%% @doc Fetch binary stored at Namespace and Key, returning `undefined` if not found.
-spec nvs_get_binary(Namespace :: atom(), Key :: atom()) -> binary() | undefined.
nvs_get_binary(Namespace, Key) when is_atom(Namespace) andalso is_atom(Key) ->
    case esp:nvs_fetch_binary(Namespace, Key) of
        {ok, Binary} -> Binary;
        {error, _Reason} -> undefined
    end.

%% @doc Fetch binary stored at Namespace and Key, returning Default if not found.
-spec nvs_get_binary(Namespace :: atom(), Key :: atom(), Default :: term()) -> term().
nvs_get_binary(Namespace, Key, Default) when is_atom(Namespace) andalso is_atom(Key) ->
    case esp:nvs_get_binary(Namespace, Key) of
        undefined -> Default;
        Binary -> Binary
    end.

%% @doc Store binary in default namespace.
-spec nvs_set_binary(Key :: atom(), Value :: binary()) -> ok.
nvs_set_binary(Key, Value) when is_atom(Key) andalso is_binary(Value) ->
    esp:nvs_put_binary(?ATOMVM_NVS_NS, Key, Value).

%% @doc Store binary in given namespace.
-spec nvs_set_binary(Namespace :: atom(), Key :: atom(), Value :: binary()) -> ok.
nvs_set_binary(Namespace, Key, Value) when
    is_atom(Namespace) andalso is_atom(Key) andalso is_binary(Value)
->
    esp:nvs_put_binary(Namespace, Key, Value).

%% @doc Store binary in NVS key-value storage.
-spec nvs_put_binary(Namespace :: atom(), Key :: atom(), Value :: binary()) -> ok.
nvs_put_binary(Namespace, Key, Value) when
    is_atom(Namespace) andalso is_atom(Key) andalso is_binary(Value)
->
    erlang:nif_error(undefined).

%% @doc Erase key from default namespace.
-spec nvs_erase_key(Key :: atom()) -> ok.
nvs_erase_key(Key) when is_atom(Key) ->
    esp:nvs_erase_key(?ATOMVM_NVS_NS, Key).

%% @doc Erase key from specified namespace.
-spec nvs_erase_key(Namespace :: atom(), Key :: atom()) -> ok.
nvs_erase_key(Namespace, Key) when is_atom(Namespace) andalso is_atom(Key) ->
    erlang:nif_error(undefined).

%% @doc Erase all keys from default namespace.
-spec nvs_erase_all() -> ok.
nvs_erase_all() ->
    esp:nvs_erase_all(?ATOMVM_NVS_NS).

%% @doc Erase all keys in specified namespace.
-spec nvs_erase_all(Namespace :: atom()) -> ok.
nvs_erase_all(Namespace) when is_atom(Namespace) ->
    erlang:nif_error(undefined).

%% @doc Reformat NVS storage partition.
-spec nvs_reformat() -> ok.
nvs_reformat() ->
    erlang:nif_error(undefined).

%%
%% Low-level SPI Flash APIs
%%

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

%%
%% Flash Filesystem (JFFS2) APIs
%%

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
