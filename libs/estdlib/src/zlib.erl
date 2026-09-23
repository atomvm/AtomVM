%
% This file is part of AtomVM.
%
% Copyright 2026 Davide Bettio <davide@uninstall.it>
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
%% @doc An implementation of a subset of the Erlang/OTP zlib interface.
%% @end
%%-----------------------------------------------------------------------------
-module(zlib).

-export([compress/1]).

%%-----------------------------------------------------------------------------
%% @param   Data  data to compress, as a binary or an iolist
%% @returns the compressed data as a binary
%% @doc     Compress data using the zlib format.
%%
%% Uses the standard zlib format (RFC 1950, with a zlib header and an Adler-32
%% checksum) at the default compression level; the result can be decompressed
%% with `zlib:uncompress/1' on Erlang/OTP.
%%
%% This function is only available when AtomVM is built with zlib support;
%% otherwise it raises `undefined'.
%% @end
%%-----------------------------------------------------------------------------
-spec compress(Data :: iodata()) -> binary().
compress(_Data) ->
    erlang:nif_error(undefined).
