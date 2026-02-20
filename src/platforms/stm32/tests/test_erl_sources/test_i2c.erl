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
%

-module(test_i2c).
-export([start/0]).

start() ->
    I2C = i2c:open([{peripheral, 1}, {scl, {b, 6}}, {sda, {b, 7}}]),
    {ok, <<16#60>>} = i2c:read_bytes(I2C, 16#76, 16#D0, 1),
    ok = i2c:write_bytes(I2C, 16#76, 16#F4, 16#27),
    {ok, <<16#27>>} = i2c:read_bytes(I2C, 16#76, 16#F4, 1),
    erlang:display(i2c_done).
