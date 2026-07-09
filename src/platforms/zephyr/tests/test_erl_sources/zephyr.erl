%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(zephyr).

-export([
    mkfs/2,
    mount/4,
    umount/1,
    socketpair/0,
    pm_state_force/2,
    pm_state_next_get/1
]).

mkfs(_Source, _FS) ->
    erlang:nif_error(undefined).

mount(_Source, _Target, _FS, _Opts) ->
    erlang:nif_error(undefined).

umount(_MountedFS) ->
    erlang:nif_error(undefined).

socketpair() ->
    erlang:nif_error(undefined).

pm_state_force(_Cpu, _StateInfo) ->
    erlang:nif_error(undefined).

pm_state_next_get(_Cpu) ->
    erlang:nif_error(undefined).
