%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Fred Dushin <fred@dushin.net>
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
%% @doc ESP32-specific APIs
%%
%% This module contains functions that are specific to the ESP32 platform.
%% @end
%%-----------------------------------------------------------------------------
-module(esp).

-export([
    restart/0,
    reset_reason/0,
    sleep_get_wakeup_cause/0,
    sleep_enable_ext0_wakeup/2,
    sleep_enable_ext1_wakeup/2,
    sleep_enable_ext1_wakeup_io/2,
    sleep_disable_ext1_wakeup_io/1,
    deep_sleep_enable_gpio_wakeup/2,
    sleep_enable_ulp_wakeup/0,
    deep_sleep/0,
    deep_sleep/1,
    mount/4,
    umount/1,
    nvs_fetch_binary/2,
    nvs_get_binary/1, nvs_get_binary/2, nvs_get_binary/3,
    nvs_set_binary/2, nvs_set_binary/3,
    nvs_put_binary/3,
    nvs_erase_key/1, nvs_erase_key/2,
    nvs_erase_all/0, nvs_erase_all/1,
    nvs_reformat/0,
    partition_erase_range/2, partition_erase_range/3,
    partition_list/0,
    partition_read/3,
    partition_write/3,
    rtc_slow_get_binary/0,
    rtc_slow_set_binary/1,
    freq_hz/0,
    get_mac/1,
    get_default_mac/0,
    task_wdt_init/1,
    task_wdt_reconfigure/1,
    task_wdt_deinit/0,
    task_wdt_add_user/1,
    task_wdt_reset_user/1,
    task_wdt_delete_user/1
]).

-deprecated([
    {nvs_get_binary, 1, next_version},
    {nvs_erase_key, 1, next_version},
    {nvs_erase_all, 0, next_version},
    {nvs_set_binary, 2, next_version},
    {nvs_set_binary, 3, next_version}
]).

-type esp_reset_reason() ::
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

-type esp_wakeup_cause() ::
    sleep_wakeup_ext0
    | sleep_wakeup_ext1
    | sleep_wakeup_timer
    | sleep_wakeup_touchpad
    | sleep_wakeup_ulp
    | sleep_wakeup_gpio
    | sleep_wakeup_uart
    | sleep_wakeup_wifi
    | sleep_wakeup_cocpu
    | sleep_wakeup_cocpu_trap_trig
    | sleep_wakeup_bt.

-type esp_partition_type() :: 0..254.
-type esp_partition_subtype() :: 0..254.
-type esp_partition_address() :: 0..134217728.
-type esp_partition_size() :: 0..134217728.
-type esp_partition_props() :: [].

-type esp_partition() :: {
    binary(),
    esp_partition_type(),
    esp_partition_subtype(),
    esp_partition_address(),
    esp_partition_size(),
    esp_partition_props()
}.

-type interface() :: wifi_sta | wifi_softap.
-type mac() :: binary().

-type task_wdt_config() :: {
    TimeoutMS :: pos_integer(),
    IdleCoreMask :: non_neg_integer(),
    TriggerPanic :: boolean()
}.
-opaque task_wdt_user_handle() :: binary().

-opaque mounted_fs() :: binary().

-export_type(
    [
        esp_reset_reason/0,
        esp_wakeup_cause/0,
        esp_partition/0,
        esp_partition_type/0,
        esp_partition_subtype/0,
        esp_partition_address/0,
        esp_partition_size/0,
        esp_partition_props/0,
        mounted_fs/0,
        task_wdt_config/0,
        task_wdt_user_handle/0
    ]
).

-define(ATOMVM_NVS_NS, atomvm).

%%-----------------------------------------------------------------------------
%% @doc     Restarts the ESP device
%% @end
%%-----------------------------------------------------------------------------
-spec restart() -> ok.
restart() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns the reason for the restart
%% @doc     Returns the reason for the restart
%% @end
%%-----------------------------------------------------------------------------
-spec reset_reason() -> esp_reset_reason().
reset_reason() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns the cause for the wake up
%% @doc     Returns the cause for the wakeup
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_get_wakeup_cause() -> undefined | esp_wakeup_cause() | error.
sleep_get_wakeup_cause() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Configure gpio wakeup from deep sleep.
%% Implemented for SOCs that support it (ESP32, ESP32S2, ESP32S3)
%% @param   Pin number of the pin to use as wakeup event
%% @param   Level is the state to trigger a wakeup
%% @returns `ok | error'
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_enable_ext0_wakeup(Pin :: non_neg_integer(), Level :: 0..1) -> ok | error.
sleep_enable_ext0_wakeup(_Pin, _Level) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Configure multiple gpio pins for wakeup from deep sleep.
%% Implemented for SOCs that support it (ESP32, ESP32S2, ESP32S3, ESP32C6, ESP32H2).
%% @deprecated This function will be removed in ESP-IDF 6.0. Use {@link sleep_enable_ext1_wakeup_io} instead.
%% @param   Mask bit mask of GPIO numbers which will cause wakeup
%% @param   Mode used to determine wakeup events
%%
%%     The available modes are:
%% <table>
%%   <tr> <th>Mode</th> <th>Description</th> </tr>
%%   <tr> <td>`0'</td> <td>WAKEUP_ALL_LOW (ESP32)</td></tr>
%%   <tr> <td>`0'</td> <td>WAKEUP_ANY_LOW (ESP32S2, ESP32S3, ESP32C6, ESP32H2)</td></tr>
%%   <tr> <td>`1'</td> <td>WAKEUP_ANY_HIGH</td></tr>
%% </table>
%% @returns `ok | error'
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_enable_ext1_wakeup(Mask :: pos_integer(), Mode :: 0..3) -> ok | error.
sleep_enable_ext1_wakeup(_Mask, _Mode) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Configure multiple gpio pins for wakeup from deep sleep.
%% Implemented for SOCs that support it (ESP32, ESP32S2, ESP32S3, ESP32C6, ESP32H2).
%% This function does not reset the previously set pins. On some SOCs (ESP32C6, ESP32H2),
%% pins can be configured with different levels.
%% @since ESP-IDF 5.3 and maybe 5.2.2
%% @param   Mask bit mask of GPIO numbers which will cause wakeup
%% @param   Mode used to determine wakeup events
%%
%%     The available modes are:
%% <table>
%%   <tr> <th>Mode</th> <th>Description</th> </tr>
%%   <tr> <td>`0'</td> <td>WAKEUP_ALL_LOW (ESP32)</td></tr>
%%   <tr> <td>`0'</td> <td>WAKEUP_ANY_LOW (ESP32S2, ESP32S3, ESP32C6, ESP32H2)</td></tr>
%%   <tr> <td>`1'</td> <td>WAKEUP_ANY_HIGH</td></tr>
%% </table>
%% @returns `ok | error'
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_enable_ext1_wakeup_io(Mask :: pos_integer(), Mode :: 0..3) -> ok | error.
sleep_enable_ext1_wakeup_io(_Mask, _Mode) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Unconfigure one or more gpio pins for wakeup from deep sleep.
%% Implemented for SOCs that support it (ESP32, ESP32S2, ESP32S3, ESP32C6, ESP32H2).
%% This function resets pins previously configured with {@link sleep_enable_ext1_wakeup_io}.
%% @since ESP-IDF 5.3 and maybe 5.2.2
%% @param   Mask bit mask of GPIO numbers to reset.
%% @returns `ok | error'
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_disable_ext1_wakeup_io(Mask :: pos_integer()) -> ok | error.
sleep_disable_ext1_wakeup_io(_Mask) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Configure multiple gpio pins for wakeup from deep sleep.
%% Implemented for SOCs that support it (ESP32C2, ESP32C3, ESP32C6, ESP32P4)
%% @param   Mask bit mask of GPIO numbers which will cause wakeup
%% @param   Mode level that will trigger the wakeup.
%%
%%     The available modes are:
%% <table>
%%   <tr> <th>Mode</th> <th>Description</th> </tr>
%%   <tr> <td>`0'</td> <td>ESP_GPIO_WAKEUP_GPIO_LOW</td></tr>
%%   <tr> <td>`1'</td> <td>ESP_GPIO_WAKEUP_GPIO_HIGH</td></tr>
%% </table>
%% @returns `ok | error'
%% @end
%%-----------------------------------------------------------------------------
-spec deep_sleep_enable_gpio_wakeup(Mask :: pos_integer(), Mode :: 0..1) -> ok | error.
deep_sleep_enable_gpio_wakeup(_Mask, _Mode) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns Enable ulp wakeup
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_enable_ulp_wakeup() -> ok | error.
sleep_enable_ulp_wakeup() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Put the esp32 into deep sleep.
%% This function never returns. Program is restarted and wake up reason can be
%% inspected to determine how the esp32 was woken up.
%% @end
%%-----------------------------------------------------------------------------
-spec deep_sleep() -> no_return().
deep_sleep() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   SleepMS time to deep sleep in milliseconds
%% @doc Put the esp32 into deep sleep.
%% This function never returns. Program is restarted and wake up reason can be
%% inspected to determine if the esp32 was woken by the timeout or by another
%% cause.
%% @end
%%-----------------------------------------------------------------------------
-spec deep_sleep(SleepMS :: non_neg_integer()) -> no_return().
deep_sleep(_SleepMS) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Source the device that will be mounted
%% @param   Target the path where the filesystem will be mounted
%% @param   FS the filesystem, only fat is supported now
%% @param   Opts
%% @returns either a tuple having `ok' and the mounted fs resource, or an error tuple
%% @doc     Mount a filesystem, and return a resource that can be used later for unmounting it
%% @end
%%-----------------------------------------------------------------------------
-spec mount(
    Source :: unicode:chardata(),
    Target :: unicode:chardata(),
    FS :: fat,
    Opts :: proplists:proplist() | #{atom() => term()}
) -> {ok, mounted_fs()} | {error, term()}.
mount(_Source, _Target, _FS, _Opts) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   The mounted filesystem resource that should be unmounted
%% @returns either `ok' or an error tuple
%% @doc     Unmounts filesystem located at given path
%% @end
%%-----------------------------------------------------------------------------
-spec umount(mounted_fs()) -> ok | {error, term()}.
umount(_Target) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Namespace NVS namespace
%% @param   Key NVS key
%% @returns tagged tuple with binary value associated with this key in NV
%%          storage, {error, not_found} if there is no value associated with
%%          this key, or in general {error, Reason} for any other error.
%% @doc     Get the binary value associated with a key, or undefined, if
%%          there is no value associated with this key.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_fetch_binary(Namespace :: atom(), Key :: atom()) ->
    {ok, binary()} | {error, not_found} | {error, atom()}.
nvs_fetch_binary(Namespace, Key) when is_atom(Namespace) andalso is_atom(Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Equivalent to nvs_get_binary(?ATOMVM_NVS_NS, Key).
%% @deprecated Please do not use this function.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_get_binary(Key :: atom()) -> binary() | undefined.
nvs_get_binary(Key) when is_atom(Key) ->
    esp:nvs_get_binary(?ATOMVM_NVS_NS, Key).

%%-----------------------------------------------------------------------------
%% @param   Namespace NVS namespace
%% @param   Key NVS key
%% @returns binary value associated with this key in NV storage, or undefined
%%          if there is no value associated with this key.
%% @doc     Get the binary value associated with a key, or undefined, if
%%          there is no value associated with this key.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_get_binary(Namespace :: atom(), Key :: atom()) -> binary() | undefined.
nvs_get_binary(Namespace, Key) when is_atom(Namespace) andalso is_atom(Key) ->
    case esp:nvs_fetch_binary(Namespace, Key) of
        {ok, Result} -> Result;
        {error, not_found} -> undefined;
        {error, namespace_not_found} -> undefined;
        {error, OtherError} -> throw(OtherError)
    end.

%%-----------------------------------------------------------------------------
%% @param   Namespace NVS namespace
%% @param   Key NVS key
%% @param   Default default binary value, if Key is not set in Namespace
%% @returns binary value associated with this key in NV storage, or Default
%%          if there is no value associated with this key.
%% @doc     Get the binary value associated with a key, or Default, if
%%          there is no value associated with this key.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_get_binary(Namespace :: atom(), Key :: atom(), Default :: binary()) ->
    binary() | undefined.
nvs_get_binary(Namespace, Key, Default) when
    is_atom(Namespace) andalso is_atom(Key) andalso is_binary(Default)
->
    case esp:nvs_get_binary(Namespace, Key) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%%-----------------------------------------------------------------------------
%% @doc Equivalent to nvs_set_binary(?ATOMVM_NVS_NS, Key, Value).
%% @deprecated Please use nvs_put_binary instead.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_set_binary(Key :: atom(), Value :: binary()) -> ok.
nvs_set_binary(Key, Value) when is_atom(Key) andalso is_binary(Value) ->
    nvs_set_binary(?ATOMVM_NVS_NS, Key, Value).

%%-----------------------------------------------------------------------------
%% @param   Namespace NVS namespace
%% @param   Key NVS key
%% @param   Value binary value
%% @returns ok
%% @doc     Set an binary value associated with a key.  If a value exists
%%          for the specified key, it is over-written.
%% @deprecated Please use nvs_put_binary instead.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_set_binary(Namespace :: atom(), Key :: atom(), Value :: binary()) -> ok.
nvs_set_binary(Namespace, Key, Value) when
    is_atom(Namespace) andalso is_atom(Key) andalso is_binary(Value)
->
    esp:nvs_put_binary(Namespace, Key, Value).

%%-----------------------------------------------------------------------------
%% @param   Namespace NVS namespace
%% @param   Key NVS key
%% @param   Value binary value
%% @returns ok
%% @doc     Set an binary value associated with a key.  If a value exists
%%          for the specified key, it is over-written.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_put_binary(Namespace :: atom(), Key :: atom(), Value :: binary()) -> ok.
nvs_put_binary(Namespace, Key, Value) when
    is_atom(Namespace) andalso is_atom(Key) andalso is_binary(Value)
->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Key NVS key
%% @returns ok
%% @doc Equivalent to nvs_erase_key(?ATOMVM_NVS_NS, Key).
%% @deprecated Please do not use this function.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_erase_key(Key :: atom()) -> ok.
nvs_erase_key(Key) when is_atom(Key) ->
    esp:nvs_erase_key(?ATOMVM_NVS_NS, Key).

%%-----------------------------------------------------------------------------
%% @param   Namespace NVS namespace
%% @param   Key NVS key
%% @returns ok
%% @doc     Erase the value associated with a key.  If a value does not exist
%%          for the specified key, no action is performed.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_erase_key(Namespace :: atom(), Key :: atom()) -> ok.
nvs_erase_key(Namespace, Key) when is_atom(Namespace) andalso is_atom(Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @doc Equivalent to nvs_erase_all(?ATOMVM_NVS_NS).
%% @deprecated Please do not use this function.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_erase_all() -> ok.
nvs_erase_all() ->
    esp:nvs_erase_all(?ATOMVM_NVS_NS).

%%-----------------------------------------------------------------------------
%% @param   Namespace NVS namespace
%% @returns ok
%% @doc     Erase all values in the specified namespace.
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_erase_all(Namespace :: atom()) -> ok.
nvs_erase_all(Namespace) when is_atom(Namespace) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns ok
%% @doc     Reformat the entire NVS partition.
%%          WARNING.  This will result in deleting all NVS data and should
%%          be used with extreme caution!
%% @end
%%-----------------------------------------------------------------------------
-spec nvs_reformat() -> ok.
nvs_reformat() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   partition_id The id of the partition to erase eg. "main.avm"
%% @param   offset Starting offset in bytes where to begin erasing
%% @returns ok on success, error on failure
%% @doc     Erases a range in the specified partition. The range starts at the given
%%          offset and continues to the end of the partition. This is equivalent to
%%          calling partition_erase_range/3 with size set to the remaining partition
%%          size from the offset.
%%
%%          Note: Erasing sets all bits in the erased range to 1s. This operation
%%          is irreversible.
%% @end
%%-----------------------------------------------------------------------------
-spec partition_erase_range(Partition_id :: binary(), Offset :: non_neg_integer()) -> ok | error.
partition_erase_range(_Partition_id, _Offset) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   partition_id The id of the partition to erase eg. "main.avm"
%% @param   offset Starting offset in bytes where to begin erasing
%% @param   size Number of bytes to erase
%% @returns ok on success, error on failure
%% @doc     Erases a range of the specified size in the partition, starting at the
%%          given offset. The size must not exceed the partition's boundaries.
%%
%%          Note: Erasing sets all bits in the erased range to 1s. This operation
%%          is irreversible. Make sure the offset and size parameters are valid for
%%          the target partition to avoid errors.
%% @end
%%-----------------------------------------------------------------------------
-spec partition_erase_range(
    Partition_id :: binary(), Offset :: non_neg_integer(), Size :: pos_integer()
) -> ok | error.
partition_erase_range(_Partition_id, _Offset, _Size) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns List of partitions
%% @doc     Gets the list of partitions as tuples, such as {name, type, subtype,
%%          offset, size, props}. Type and subtype are integers as described in
%%          esp-idf documentation.
%% @end
%%-----------------------------------------------------------------------------
-spec partition_list() -> [esp_partition()].
partition_list() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   partition_id The id of the partition to read from eg. "main.avm"
%% @param   offset Starting offset in bytes where to begin reading
%% @param   read_size Number of bytes to read
%% @returns {ok, data} on success, error on failure
%% @doc     Read binary data from a specific partition at the given offset.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec partition_read(
    Partition_id :: binary(), Offset :: non_neg_integer(), Read_size :: non_neg_integer()
) -> {ok, binary()} | error.
partition_read(_Partition_id, _Offset, _Read_size) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   partition_id The id of the partition to write to eg. "main.avm"
%% @param   offset Starting offset in bytes where to begin writing
%% @param   data Binary data to write to the partition
%% @returns ok on success, error on failure
%% @doc     Writes binary data to a specific partition at the given offset.
%%
%%          This function allows writing data to ESP32 flash partitions. Care must be
%%          taken when using this function as improper writes can corrupt the partition
%%          table or system firmware. Make sure the offset and data size do not exceed
%%          the partition's boundaries.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec partition_write(Partition_id :: binary(), Offset :: non_neg_integer(), Data :: binary()) ->
    ok | error.
partition_write(_Partition_id, _Offset, _Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns the currently stored binary in RTC slow memory.
%% @doc     Get the binary currently stored in RTC slow memory. Must not be
%%          called unless the binary was stored with rtc_slow_set_binary/1.
%%          A limited checksum is ran and this function may throw badarg if
%%          the checksum is not valid.
%% @end
%%-----------------------------------------------------------------------------
-spec rtc_slow_get_binary() -> binary().
rtc_slow_get_binary() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Bin binary to be stored in RTC slow memory
%% @returns ok
%% @doc     Store a binary to RTC slow memory. This memory is not erased on
%%          software reset and deep sleeps.
%% @end
%%-----------------------------------------------------------------------------
-spec rtc_slow_set_binary(Bin :: binary()) -> ok.
rtc_slow_set_binary(Bin) when is_binary(Bin) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns Clock frequency (in hz)
%% @doc     Return the clock frequency on the chip
%% @end
%%-----------------------------------------------------------------------------
-spec freq_hz() -> non_neg_integer().
freq_hz() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Interface the ESP32 network interface
%% @returns The network MAC address of the specified interface
%% @doc     Return the network MAC address of the specified interface.
%%
%%          The mac address is returned as a 6-byte binary, per the
%%          IEEE 802 family of specifications.
%% @end
%%-----------------------------------------------------------------------------
-spec get_mac(Interface :: interface()) -> mac().
get_mac(_Interface) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns The default MAC address of the ESP32 device.
%% @doc     Retrieve the default MAC address of the ESP32 device.
%%          This function accesses the EFUSE memory of the ESP32 and reads
%%          the factory-programmed MAC address.
%%
%%          The mac address is returned as a 6-byte binary, per the
%%          IEEE 802 family of specifications.
%% @end
%%-----------------------------------------------------------------------------
-spec get_default_mac() -> {ok, mac()} | {error, atom()}.
get_default_mac() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Config configuration for the watchdog timer
%% @returns ok or an error tuple
%% @doc     Initialize the task watchdog timer with a configuration
%%          Available with ESP-IDF 5.0 or higher.
%% @end
%%-----------------------------------------------------------------------------
-spec task_wdt_init(Config :: task_wdt_config()) -> ok | {error, already_started} | {error, any()}.
task_wdt_init(_Config) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Config configuration for the watchdog timer
%% @returns ok or an error tuple
%% @doc     Update the configuration of the task watchdog timer
%%          Available with ESP-IDF 5.0 or higher.
%% @end
%%-----------------------------------------------------------------------------
-spec task_wdt_reconfigure(Config :: task_wdt_config()) -> ok | {error, noproc} | {error, any()}.
task_wdt_reconfigure(_Config) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns ok or an error tuple if tasks are subscribed (beyond idle tasks) or
%%          if the timer is not initialized
%% @doc     Deinitialize the task watchdog timer
%%          Available with ESP-IDF 5.0 or higher.
%% @end
%%-----------------------------------------------------------------------------
-spec task_wdt_deinit() -> ok | {error, any()}.
task_wdt_deinit() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Username name of the user
%% @returns the handle to use with `task_wdt_reset_user/1' or an error tuple.
%% @doc     Register a user of the task watchdog timer.
%%          Available with ESP-IDF 5.0 or higher.
%% @end
%%-----------------------------------------------------------------------------
-spec task_wdt_add_user(Username :: iodata()) -> {ok, task_wdt_user_handle()} | {error, any()}.
task_wdt_add_user(_Username) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param UserHandle handle for the user, obtained from `task_wdt_add_user/1'
%% @returns ok or an error tuple
%% @doc     Reset the timer a previously registered user.
%%          Available with ESP-IDF 5.0 or higher.
%% @end
%%-----------------------------------------------------------------------------
-spec task_wdt_reset_user(UserHandle :: task_wdt_user_handle()) -> ok | {error, any()}.
task_wdt_reset_user(_UserHandle) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param UserHandle handle for the user, obtained from `task_wdt_add_user/1'
%% @returns ok or an error tuple
%% @doc     Unsubscribe a given user from the task watchdog timer.
%%          Available with ESP-IDF 5.0 or higher.
%% @end
%%-----------------------------------------------------------------------------
-spec task_wdt_delete_user(UserHandle :: task_wdt_user_handle()) -> ok | {error, any()}.
task_wdt_delete_user(_UserHandle) ->
    erlang:nif_error(undefined).
