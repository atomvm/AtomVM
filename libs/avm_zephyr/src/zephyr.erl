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
%% reboot, reset cause, MAC address, boot time, filesystem mount helpers,
%% POSIX socketpair, power-management state forcing, and the task watchdog.
-module(zephyr).

-export([
    restart/0,
    reset_reason/0,
    get_mac/1,
    get_default_mac/0,
    timer_get_time/0,
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
    interface/0,
    mac/0,
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
-type interface() :: default | wifi_sta.
-type mac() :: binary().

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
