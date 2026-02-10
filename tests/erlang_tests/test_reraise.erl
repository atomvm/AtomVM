%
% This file is part of AtomVM.
%
% Copyright 2025 Software Mansion
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

-module(test_reraise).

-export([start/0]).

start() ->
    start(?OTP_RELEASE).

start(OTPVersion) when OTPVersion >= 23 ->
    try
        reraise_reraiser:reraise_error()
    catch
        Class:Reason:Stacktrace ->
            error = Class,
            "foo" = Reason,

            [
                {reraise_raiser, raise_error, 0, _Meta1},
                {reraise_reraiser, reraise_error, 0, _Meta2}
                | _Rest
            ] = Stacktrace
    end,
    0;
start(_) ->
    0.
