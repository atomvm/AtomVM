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

-module(test_gpio).
-export([start/0]).

start() ->
    gpio:set_pin_mode({a, 5}, output),
    gpio:digital_write({a, 5}, 1),
    erlang:display(gpio_high),
    gpio:digital_write({a, 5}, 0),
    erlang:display(gpio_low),
    gpio:digital_write({a, 5}, 1),
    erlang:display(gpio_done).
