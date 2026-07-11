%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% @doc Portable retained-memory storage for Zephyr.
%%
%% The backing area is selected by the `atomvm,retention' devicetree chosen
%% node. Offsets and sizes address the usable area after any configured prefix
%% and checksum.
-module(retention).

-export([size/0, is_valid/0, read/2, write/2, clear/0]).

-spec size() -> non_neg_integer() | {error, term()}.
size() -> erlang:nif_error(undefined).

-spec is_valid() -> boolean() | {error, term()}.
is_valid() -> erlang:nif_error(undefined).

-spec read(non_neg_integer(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
read(_Offset, _Size) -> erlang:nif_error(undefined).

-spec write(non_neg_integer(), binary()) -> ok | {error, term()}.
write(_Offset, _Data) -> erlang:nif_error(undefined).

-spec clear() -> ok | {error, term()}.
clear() -> erlang:nif_error(undefined).
