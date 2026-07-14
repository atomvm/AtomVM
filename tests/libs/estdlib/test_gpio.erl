%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(test_gpio).

-export([test/0]).

%% The sysfs GPIO `gpio' module is generic_unix-only
test() ->
    case
        erlang:system_info(machine) == "ATOM" andalso
            atomvm:platform() == generic_unix
    of
        true ->
            ok = test_missing_base_no_crash(),
            ok = test_set_pin_mode_error_propagates(),
            ok = test_roundtrip(),
            ok;
        false ->
            io:format("test_gpio: not supported on this platform, skipping~n"),
            ok
    end.

%% A base that does not exist: every public function must return a clean error
%% tuple, never crash.
test_missing_base_no_crash() ->
    ok = gpio:set_sysfs_base("/tmp/atomvm_test_gpio_does_not_exist"),
    {error, _} = gpio:init(5),
    {error, _} = gpio:digital_read(5),
    {error, _} = gpio:digital_write(5, high),
    ok.

%% With a non-existent base, `init/1' fails to write <base>/export;
%% `set_pin_mode/2' must propagate that error rather than crash with a
%% `{badmatch, {error, _}}' on `ok = init(Pin)'.
test_set_pin_mode_error_propagates() ->
    ok = gpio:set_sysfs_base("/tmp/atomvm_test_gpio_does_not_exist"),
    {error, _} = gpio:set_pin_mode(5, output),
    ok.

%% Happy path against a fabricated sysfs tree.
test_roundtrip() ->
    Pin = 17,
    Base = "/tmp/atomvm_test_gpio_sysfs",
    ok = setup_fake_sysfs(Base, Pin),
    ok = gpio:set_sysfs_base(Base),
    ok = gpio:set_pin_mode(Pin, output),
    ok = gpio:digital_write(Pin, high),
    high = gpio:digital_read(Pin),
    ok = gpio:digital_write(Pin, low),
    low = gpio:digital_read(Pin),
    %% integer levels are accepted too
    ok = gpio:digital_write(Pin, 1),
    high = gpio:digital_read(Pin),
    ok = gpio:digital_write(Pin, 0),
    low = gpio:digital_read(Pin),
    ok.

%% Build <base>/{export,unexport} and <base>/gpio<Pin>/{direction,value} as
%% regular files so the driver can open them the way it would open the real
%% sysfs attributes.
setup_fake_sysfs(Base, Pin) ->
    _ = atomvm:posix_mkdir(Base, 8#755),
    ok = create_empty_file(Base ++ "/export"),
    ok = create_empty_file(Base ++ "/unexport"),
    PinDir = Base ++ "/gpio" ++ integer_to_list(Pin),
    _ = atomvm:posix_mkdir(PinDir, 8#755),
    ok = create_empty_file(PinDir ++ "/direction"),
    ok = create_empty_file(PinDir ++ "/value"),
    ok.

create_empty_file(Path) ->
    {ok, Fd} = atomvm:posix_open(Path, [o_wronly, o_creat, o_trunc], 8#644),
    ok = atomvm:posix_close(Fd),
    ok.
