%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(datetime).

-export([start/0]).

start() ->
    {Date, Time} = erlang:universaltime(),
    test_date(Date) + test_time(Time).

test_date({Y, M, D}) when Y >= 2018 andalso M >= 1 andalso M =< 12 andalso D >= 1 andalso D =< 31 ->
    1.

test_time({HOUR, MIN, SEC}) when
    HOUR >= 0 andalso HOUR < 24 andalso MIN >= 0 andalso MIN < 60 andalso SEC >= 0 andalso MIN < 60
->
    2.
