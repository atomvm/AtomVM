%
% This file is part of AtomVM.
%
% Copyright 2025 schnittchen <schnittchen@das-labor.org>
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

-module(test_esp_timer_get_time).

-export([start/0]).

start() ->
    T = esp:timer_get_time(),
    test_non_neg_int(T).

test_non_neg_int(X) when is_integer(X) andalso X >= 0 ->
    ok;
test_non_neg_int(_X) ->
    error.
