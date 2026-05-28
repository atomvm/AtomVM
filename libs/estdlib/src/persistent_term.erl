%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
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
%% @doc A limited implementation of the Erlang/OTP `persistent_term' module.
%%
%% Values are stored globally and reads return stored values without copying.
%% Replaced or erased values remain allocated until VM shutdown so references
%% already returned to processes stay valid without a global GC pass. The
%% `memory' value returned by `info/0' includes those retained old values.
%% @end
%%-----------------------------------------------------------------------------
-module(persistent_term).

-export([erase/1, get/0, get/1, get/2, info/0, put/2, put_new/2]).

-export_type([key/0, value/0]).

-type key() :: term().
-type value() :: term().

-spec erase(Key :: key()) -> boolean().
erase(_Key) ->
    erlang:nif_error(undefined).

-spec get() -> [{key(), value()}].
get() ->
    erlang:nif_error(undefined).

-spec get(Key :: key()) -> value().
get(_Key) ->
    erlang:nif_error(undefined).

-spec get(Key :: key(), Default :: value()) -> value().
get(_Key, _Default) ->
    erlang:nif_error(undefined).

-spec info() -> #{count := non_neg_integer(), memory := non_neg_integer()}.
info() ->
    erlang:nif_error(undefined).

-spec put(Key :: key(), Value :: value()) -> ok.
put(_Key, _Value) ->
    erlang:nif_error(undefined).

-spec put_new(Key :: key(), Value :: value()) -> ok.
put_new(_Key, _Value) ->
    erlang:nif_error(undefined).
