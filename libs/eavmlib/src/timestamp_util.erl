%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Fred Dushin <fred@dushin.net>
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
%% @doc Utility functions for comparing timestamps.
%%
%% This module contains functions that are useful for comparing
%% timestamps without running the risk of integer overflow.
%%
%% Note that the functions in this module may be obsoleted in
%% future versions of AtomVM, as support for arbitrary sized integers
%% is added; however, the functions may still be useful in their own right.
%% @end
%%-----------------------------------------------------------------------------
-module(timestamp_util).

-export([delta/2, delta_ms/2]).

-define(SECS_PER_MEGASECS, 1000000).
-define(MILLSECES_MER_MICROSECS, 1000).
-define(MILLSECS_PER_SECS, 1000).

-type megasecs() :: integer().
-type secs() :: integer().
-type microsecs() :: integer().
-type timestamp() :: {megasecs(), secs(), microsecs()}.

%%-----------------------------------------------------------------------------
%% @param   TS2 a timestamp
%% @param   TS1 a timestamp
%% @returns TS2 - TS1, in milliseconds
%% @doc     Computes the difference between TS2 and TS1, in milliseconds.
%% @end
%%-----------------------------------------------------------------------------
-spec delta_ms(timestamp(), timestamp()) -> integer().
delta_ms(TS2, TS1) ->
    timestamp_to_ms(delta(TS2, TS1)).

%%-----------------------------------------------------------------------------
%% @param   TS2 a timestamp
%% @param   TS1 a timestamp
%% @returns TS2 - TS1, as a timestamp
%% @doc     Computes the difference between TS2 and TS1, as a timestamp.
%% @end
%%-----------------------------------------------------------------------------
-spec delta(TS2 :: timestamp(), TS1 :: timestamp()) -> timestamp().
delta({Mega2, Sec2, Micro2}, {Mega1, Sec1, Micro1}) ->
    {SDelta, Micros} = minus(Micro2, Micro1, 1000000),
    {MegaDelta, Secs} = minus(Sec2 + SDelta, Sec1, 1000000),
    Megas = (Mega2 + MegaDelta) - Mega1,
    {Megas, Secs, Micros}.

%%
%% Internal operations
%%

%% @private
minus(A, B, _F) when A >= B ->
    {0, A - B};
minus(A, B, F) ->
    {-1, F - (B - A)}.

%% @private
timestamp_to_ms({Mega, Sec, Micro}) ->
    Secs = Mega * ?SECS_PER_MEGASECS + Sec,
    Secs * ?MILLSECS_PER_SECS + Micro div ?MILLSECES_MER_MICROSECS.
