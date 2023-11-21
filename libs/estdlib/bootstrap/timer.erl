%
% This file is part of AtomVM.
%
% Copyright 2018-2021 Davide Bettio <davide@uninstall.it>
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

%%-----------------------------------------------------------------------------
%% @doc An implementation of the Erlang/OTP timer interface.
%%
%% This module implements a strict subset of the Erlang/OTP timer
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(timer).

-export([sleep/1]).

%%-----------------------------------------------------------------------------
%% @param Timeout number of milliseconds to sleep or `infinity'
%% @returns `ok'
%%
%% @doc Pauses the execution of the current process for a given number of
%%      milliseconds, or forever, using `infinity' as the parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec sleep(Timeout :: timeout()) -> ok.
sleep(Timeout) ->
    receive
    after Timeout ->
        ok
    end.
