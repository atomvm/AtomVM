%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(test_mount).
-export([start/0]).

start() ->
    {ok, Ref} = mount_working_sdmmc(),
    ok = umount_prev(Ref),
    ok = mount_missing_fat_partition(),
    ok = umount_prev(Ref).

mount_working_sdmmc() ->
    {ok, Ref} = esp:mount("sdmmc", "/test", fat, []),
    {ok, Fd} = atomvm:posix_open("/test/test.txt", [o_rdwr, o_creat], 8#644),
    ok = atomvm:posix_close(Fd),
    ok = esp:umount(Ref),
    {ok, Ref}.

mount_missing_fat_partition() ->
    {error, esp_err_not_found} = esp:mount("/dev/partition/by-name/missingpart", "/test", fat, []),
    ok.

umount_prev(Ref) ->
    try esp:umount(Ref) of
        ok -> error
    catch
        error:badarg -> ok;
        _:_ -> not_badarg
    end.
