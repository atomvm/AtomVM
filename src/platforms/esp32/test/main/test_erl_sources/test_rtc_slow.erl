%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_rtc_slow).
-export([start/0]).

-define(CONFIG_AVM_RTC_SLOW_MAX_SIZE, 1024).

% Simply ensure rtc_slow nifs can be called
start() ->
    % During qemu tests, RTC Slow memory is initialized to 0.
    % So the checksum is always wrong.
    ok =
        try
            esp:rtc_slow_get_binary()
        catch
            error:badarg -> ok
        end,
    ok = esp:rtc_slow_set_binary(<<>>),
    <<>> = esp:rtc_slow_get_binary(),
    ok = esp:rtc_slow_set_binary(<<42>>),
    <<42>> = esp:rtc_slow_get_binary(),
    LargeBin16K = build_bin(16 * 1024),
    ok =
        try
            esp:rtc_slow_set_binary(LargeBin16K),
            should_fail
        catch
            error:badarg -> ok
        end,
    <<42>> = esp:rtc_slow_get_binary(),
    LargeBinMax = build_bin(?CONFIG_AVM_RTC_SLOW_MAX_SIZE),
    ok = esp:rtc_slow_set_binary(LargeBinMax),
    LargeBinMaxPlusOne = build_bin(?CONFIG_AVM_RTC_SLOW_MAX_SIZE + 1),
    ok =
        try
            esp:rtc_slow_set_binary(LargeBinMaxPlusOne),
            should_fail
        catch
            error:badarg -> ok
        end,
    LargeBinMax = esp:rtc_slow_get_binary(),
    % Make sure get/set is semantically correct, even with binaries that do not fit the heap
    esp:rtc_slow_set_binary(<<42:1024>>),
    X = esp:rtc_slow_get_binary(),
    true = id(X) =:= <<42:1024>>,
    esp:rtc_slow_set_binary(<<43:1024>>),
    Y = esp:rtc_slow_get_binary(),
    true = id(Y) =:= <<43:1024>>,
    true = id(X) =:= <<42:1024>>,
    0.

build_bin(Len) ->
    <<42:(Len * 8)>>.

id(X) -> X.
