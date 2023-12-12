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
    sleep_enable_ulp_wakeup/0,
    deep_sleep/0,
    deep_sleep/1,
    nvs_fetch_binary/2,
    nvs_get_binary/1, nvs_get_binary/2, nvs_get_binary/3,
    nvs_set_binary/2, nvs_set_binary/3,
    nvs_put_binary/3,
    nvs_erase_key/1, nvs_erase_key/2,
    nvs_erase_all/0, nvs_erase_all/1,
    nvs_reformat/0,
    partition_list/0,
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

-export_type(
    [
        esp_partition/0,
        esp_partition_type/0,
        esp_partition_subtype/0,
        esp_partition_address/0,
        esp_partition_size/0,
        esp_partition_props/0,
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
%% @returns Configure ext0 wakeup
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_enable_ext0_wakeup(Pin :: pos_integer(), Level :: 0..1) -> ok | error.
sleep_enable_ext0_wakeup(_Pin, _Level) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns Configure ext1 wakeup
%% @end
%%-----------------------------------------------------------------------------
-spec sleep_enable_ext1_wakeup(Mask :: non_neg_integer(), Mode :: 0..1) -> ok | error.
sleep_enable_ext1_wakeup(_Mask, _Mode) ->
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
