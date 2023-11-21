%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

%%
%% Copyright (c) dushin.net
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%%-----------------------------------------------------------------------------
%% @doc An implementation of a subset of the Erlang/OTP base64 interface.
%%
%% This module is designed to be API-compatible with the Erlang/OTP base64 module,
%% with the following exceptions:
%%
%% <ul>
%%     <li>No support for decoding data with whitespace in base64 data</li>
%%     <li>No support for mime decoding functions</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(base64).

-export([encode/1, encode_to_string/1, decode/1, decode_to_string/1]).

%%-----------------------------------------------------------------------------
%% @param   Data the data to encode
%% @returns the base-64 data encoded, as a binary
%% @doc     Base-64 encode a binary or string, outputting a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec encode(binary() | iolist()) -> binary().
encode(Data) when is_binary(Data) orelse is_list(Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data the data to encode
%% @returns the base-64 data encoded, as a string
%% @doc     Base-64 encode a binary or string, outputting a string.
%% @end
%%-----------------------------------------------------------------------------
-spec encode_to_string(binary() | iolist()) -> string().
encode_to_string(Data) when is_binary(Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data the data to decode
%% @returns the base-64 data decoded, as a binary
%% @doc     Base-64 decode a binary or string, outputting a binary.
%%
%%          This function will raise a badarg exception if the supplied
%%          data is not valid base64-encoded data.
%% @end
%%-----------------------------------------------------------------------------
-spec decode(binary() | iolist()) -> binary().
decode(Data) when is_binary(Data) orelse is_list(Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data the data to decode
%% @returns the base-64 data decoded, as a string
%% @doc     Base-64 decode a binary or string, outputting a string.
%%
%%          This function will raise a badarg exception if the supplied
%%          data is not valid base64-encoded data.
%% @end
%%-----------------------------------------------------------------------------
-spec decode_to_string(binary() | iolist()) -> string().
decode_to_string(Data) when is_binary(Data) ->
    erlang:nif_error(undefined).
