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
%% Terms remain allocated until VM shutdown. Erasing or replacing stored values
%% is not supported. Both `put/2' and `put_new/2' raise `badarg' if the key
%% already has a different value; storing the same value again returns `ok'.
%% @end
%%-----------------------------------------------------------------------------
-module(persistent_term).

-export([get/0, get/1, get/2, info/0, put/2, put_new/2]).

-export_type([key/0, value/0]).

-type key() :: term().
-type value() :: term().

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
