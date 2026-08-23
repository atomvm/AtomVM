%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc Zephyr-specific AtomVM APIs.
%%
%% This module wraps platform NIFs that are not part of a portable HAL:
%% reboot, reset cause, MAC address, boot time, persistent settings,
%% flash map, filesystem mount helpers, POSIX socketpair,
%% power-management state forcing, deep sleep / wakeup, and the task
%% watchdog.
-module(zephyr).

-export([
    restart/0,
    reset_reason/0,
    get_mac/1,
    get_default_mac/0,
    timer_get_time/0,
    deep_sleep/0,
    deep_sleep/1,
    sleep_enable_gpio_wakeup/2,
    sleep_get_wakeup_cause/0,
    settings_get/2,
    settings_get/3,
    settings_put/3,
    settings_erase/2,
    flash_list/0,
    flash_read/3,
    flash_write/3,
    flash_erase/2,
    flash_erase/3,
    flash_mmap/3,
    mkfs/2,
    mount/4,
    umount/1,
    socketpair/0,
    pm_state_force/2,
    pm_state_next_get/1,
    task_wdt_init/1,
    task_wdt_reconfigure/1,
    task_wdt_deinit/0,
    task_wdt_add_user/1,
    task_wdt_reset_user/1,
    task_wdt_delete_user/1
]).

-export_type([
    mounted_fs/0,
    posix_fd/0,
    pm_state/0,
    pm_state_info/0,
    reset_reason/0,
    wakeup_cause/0,
    gpio_wakeup_level/0,
    interface/0,
    mac/0,
    flash_area/0,
    flash_area_id/0,
    task_wdt_config/0,
    task_wdt_user_handle/0
]).

-opaque mounted_fs() :: binary().
-opaque posix_fd() :: binary().
-type pm_state() ::
    active
    | runtime_idle
    | suspend_to_idle
    | standby
    | suspend_to_ram
    | suspend_to_disk
    | soft_off
    | unknown.
-type pm_state_info() :: {
    pm_state(),
    SubstateId :: non_neg_integer(),
    MinResidencyUs :: non_neg_integer(),
    ExitLatencyUs :: non_neg_integer()
}.
-type task_wdt_config() :: {
    TimeoutMS :: pos_integer(),
    IdleCoreMask :: non_neg_integer(),
    TriggerPanic :: boolean()
}.
-opaque task_wdt_user_handle() :: binary().
-type reset_reason() ::
    pin
    | software
    | brownout
    | por
    | watchdog
    | debug
    | security
    | low_power_wake
    | cpu_lockup
    | parity
    | pll
    | clock
    | hardware
    | user
    | temperature
    | bootloader
    | flash
    | unknown.
-type wakeup_cause() :: undefined | timer | gpio | reset | unknown.
-type gpio_wakeup_level() :: 0 | 1 | low | high.
-type interface() :: default | wifi_sta.
-type mac() :: binary().
-type flash_area_id() :: non_neg_integer() | iodata().
-type flash_area() :: #{
    id := non_neg_integer(),
    offset := non_neg_integer(),
    size := non_neg_integer(),
    label => binary() | undefined
}.

-spec restart() -> no_return().
restart() ->
    erlang:nif_error(undefined).

-spec reset_reason() -> [reset_reason()] | undefined.
reset_reason() ->
    erlang:nif_error(undefined).

-spec get_mac(Interface :: interface()) -> mac().
get_mac(_Interface) ->
    erlang:nif_error(undefined).

-spec get_default_mac() -> {ok, mac()} | {error, atom()}.
get_default_mac() ->
    erlang:nif_error(undefined).

-spec timer_get_time() -> non_neg_integer().
timer_get_time() ->
    erlang:nif_error(undefined).

%% @doc Enter deep sleep. Does not return on boards that support power-off.
-spec deep_sleep() -> no_return() | {error, not_supported | term()}.
deep_sleep() ->
    erlang:nif_error(undefined).

%% @doc Arm a millisecond timer wakeup and enter deep sleep.
-spec deep_sleep(TimeoutMs :: non_neg_integer()) -> no_return() | {error, not_supported | term()}.
deep_sleep(_TimeoutMs) ->
    erlang:nif_error(undefined).

%% @doc Wake from deep sleep when Pin is at Level.
%%
%% Pin is a GPIO number on the default controller, or `{Controller, Pin}'.
%% Level is `0'/`low' or `1'/`high'.
-spec sleep_enable_gpio_wakeup(Pin :: non_neg_integer() | {term(), non_neg_integer()}, Level :: gpio_wakeup_level()) ->
    ok | {error, term()}.
sleep_enable_gpio_wakeup(_Pin, _Level) ->
    erlang:nif_error(undefined).

%% @doc Why the last boot happened after sleep, if known.
-spec sleep_get_wakeup_cause() -> wakeup_cause().
sleep_get_wakeup_cause() ->
    erlang:nif_error(undefined).

-spec settings_get(Namespace :: atom(), Key :: atom()) -> {ok, binary()} | {error, not_found | term()}.
settings_get(_Namespace, _Key) ->
    erlang:nif_error(undefined).

-spec settings_get(Namespace :: atom(), Key :: atom(), Default :: binary()) -> binary().
settings_get(Namespace, Key, Default) when is_binary(Default) ->
    case ?MODULE:settings_get(Namespace, Key) of
        {ok, Value} -> Value;
        {error, not_found} -> Default
    end.

-spec settings_put(Namespace :: atom(), Key :: atom(), Value :: binary()) -> ok | {error, term()}.
settings_put(_Namespace, _Key, _Value) ->
    erlang:nif_error(undefined).

-spec settings_erase(Namespace :: atom(), Key :: atom()) -> ok | {error, term()}.
settings_erase(_Namespace, _Key) ->
    erlang:nif_error(undefined).

%% @doc List fixed flash partitions from Zephyr's Flash Map.
-spec flash_list() -> [flash_area()].
flash_list() ->
    erlang:nif_error(undefined).

%% @doc Read Size bytes from Area at Offset.
%%
%% Area is a flash-map id or, when labels are enabled, a partition label
%% such as `<<"storage">>' or `<<"image-scratch">>'.
-spec flash_read(Area :: flash_area_id(), Offset :: non_neg_integer(), Size :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
flash_read(_Area, _Offset, _Size) ->
    erlang:nif_error(undefined).

%% @doc Write Data to Area at Offset. The range must already be erased.
-spec flash_write(Area :: flash_area_id(), Offset :: non_neg_integer(), Data :: binary()) ->
    ok | {error, term()}.
flash_write(_Area, _Offset, _Data) ->
    erlang:nif_error(undefined).

%% @doc Erase from Offset to the end of Area.
-spec flash_erase(Area :: flash_area_id(), Offset :: non_neg_integer()) -> ok | {error, term()}.
flash_erase(_Area, _Offset) ->
    erlang:nif_error(undefined).

%% @doc Erase Size bytes of Area starting at Offset.
-spec flash_erase(Area :: flash_area_id(), Offset :: non_neg_integer(), Size :: non_neg_integer()) ->
    ok | {error, term()}.
flash_erase(_Area, _Offset, _Size) ->
    erlang:nif_error(undefined).

%% @doc Map Size bytes of Area at Offset as a binary.
%%
%% Available on Espressif SoCs via `spi_flash_mmap`. The binary stays valid
%% until it is garbage collected. Other platforms return `{error, not_supported}'.
-spec flash_mmap(Area :: flash_area_id(), Offset :: non_neg_integer(), Size :: non_neg_integer()) ->
    {ok, binary()} | {error, term()}.
flash_mmap(_Area, _Offset, _Size) ->
    erlang:nif_error(undefined).

-spec mkfs(Source :: iodata(), fat) -> ok | {error, term()}.
mkfs(_Source, _FS) ->
    erlang:nif_error(undefined).

-spec mount(Source :: iodata(), Target :: iodata(), fat, Options :: term()) ->
    {ok, mounted_fs()} | {error, term()}.
mount(_Source, _Target, _FS, _Opts) ->
    erlang:nif_error(undefined).

-spec umount(MountedFS :: mounted_fs()) -> ok | {error, term()}.
umount(_MountedFS) ->
    erlang:nif_error(undefined).

-spec socketpair() -> {ok, {posix_fd(), posix_fd()}} | {error, term()}.
socketpair() ->
    erlang:nif_error(undefined).

-spec pm_state_force(Cpu :: non_neg_integer(), StateInfo :: pm_state() | tuple()) -> boolean().
pm_state_force(_Cpu, _StateInfo) ->
    erlang:nif_error(undefined).

-spec pm_state_next_get(Cpu :: non_neg_integer()) -> pm_state_info() | undefined.
pm_state_next_get(_Cpu) ->
    erlang:nif_error(undefined).

-spec task_wdt_init(Config :: task_wdt_config()) -> ok | {error, already_started} | {error, any()}.
task_wdt_init(_Config) ->
    erlang:nif_error(undefined).

-spec task_wdt_reconfigure(Config :: task_wdt_config()) -> ok | {error, noproc} | {error, any()}.
task_wdt_reconfigure(_Config) ->
    erlang:nif_error(undefined).

-spec task_wdt_deinit() -> ok | {error, any()}.
task_wdt_deinit() ->
    erlang:nif_error(undefined).

-spec task_wdt_add_user(Username :: iodata()) -> {ok, task_wdt_user_handle()} | {error, any()}.
task_wdt_add_user(_Username) ->
    erlang:nif_error(undefined).

-spec task_wdt_reset_user(UserHandle :: task_wdt_user_handle()) -> ok | {error, any()}.
task_wdt_reset_user(_UserHandle) ->
    erlang:nif_error(undefined).

-spec task_wdt_delete_user(UserHandle :: task_wdt_user_handle()) -> ok | {error, any()}.
task_wdt_delete_user(_UserHandle) ->
    erlang:nif_error(undefined).
