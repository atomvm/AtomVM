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
%%     <li>No support for the `mode' option (`urlsafe' encoding)</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-module(base64).

-export([
    encode/1, encode/2,
    encode_to_string/1, encode_to_string/2,
    decode/1, decode/2,
    decode_to_string/1, decode_to_string/2
]).

-type encode_options() :: #{padding => boolean()}.
%% Options for encoding functions.
%%
%% <ul>
%%     <li>`padding': when `true' (default), appends `=' padding characters to
%%         the encoded output so its length is a multiple of 4.  When `false',
%%         trailing `=' characters are omitted.</li>
%% </ul>

-type decode_options() :: #{padding => boolean()}.
%% Options for decoding functions.
%%
%% <ul>
%%     <li>`padding': when `true' (default), the input must be padded with the
%%         correct number of `=' characters.  When `false', missing `='
%%         padding is accepted, though padded input is still accepted too.</li>
%% </ul>

%%-----------------------------------------------------------------------------
%% @param   Data    the data to encode
%% @returns the base-64 encoded data as a binary
%% @doc     Equivalent to `encode(Data, #{})'.
%% @end
%%-----------------------------------------------------------------------------
-spec encode(Data :: binary() | iolist()) -> binary().
encode(Data) when is_binary(Data) orelse is_list(Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data    the data to encode
%% @param   Options encoding options
%% @returns the base-64 encoded data as a binary
%% @doc     Base-64 encode a binary or string, returning a binary.
%%
%%          The `padding' option controls whether `=' padding characters are
%%          appended.  The default is `#{padding => true}'.
%%
%%          Note: the `mode' option is not supported; only the standard
%%          alphabet (RFC 4648 Section 4) is available.
%% @end
%%-----------------------------------------------------------------------------
-spec encode(Data :: binary() | iolist(), Options :: encode_options()) -> binary().
encode(Data, Options) when (is_binary(Data) orelse is_list(Data)) andalso is_map(Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data    the data to encode
%% @returns the base-64 encoded data as a string
%% @doc     Equivalent to `encode_to_string(Data, #{})'.
%% @end
%%-----------------------------------------------------------------------------
-spec encode_to_string(Data :: binary() | iolist()) -> string().
encode_to_string(Data) when is_binary(Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data    the data to encode
%% @param   Options encoding options
%% @returns the base-64 encoded data as a string
%% @doc     Base-64 encode a binary or string, returning a string.
%%
%%          The `padding' option controls whether `=' padding characters are
%%          appended.  The default is `#{padding => true}'.
%%
%%          Note: the `mode' option is not supported; only the standard
%%          alphabet (RFC 4648 Section 4) is available.
%% @end
%%-----------------------------------------------------------------------------
-spec encode_to_string(Data :: binary() | iolist(), Options :: encode_options()) -> string().
encode_to_string(Data, Options) when is_binary(Data) andalso is_map(Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data    the base-64 encoded data to decode
%% @returns the decoded data as a binary
%% @doc     Equivalent to `decode(Data, #{})'.
%%
%%          Raises a `badarg' exception if the input is not valid
%%          base-64-encoded data.  Whitespace in the input is not accepted.
%% @end
%%-----------------------------------------------------------------------------
-spec decode(Data :: binary() | iolist()) -> binary().
decode(Data) when is_binary(Data) orelse is_list(Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data    the base-64 encoded data to decode
%% @param   Options decoding options
%% @returns the decoded data as a binary
%% @doc     Base-64 decode a binary or string, returning a binary.
%%
%%          When `#{padding => true}' (default), the input must carry the
%%          correct number of `=' padding characters.  When
%%          `#{padding => false}', missing `=' padding is tolerated; padded
%%          input is still accepted.
%%
%%          Raises a `badarg' exception if the input is not valid
%%          base-64-encoded data.  Whitespace in the input is not accepted.
%%
%%          Note: the `mode' option is not supported; only the standard
%%          alphabet (RFC 4648 Section 4) is available.
%% @end
%%-----------------------------------------------------------------------------
-spec decode(Data :: binary() | iolist(), Options :: decode_options()) -> binary().
decode(Data, Options) when (is_binary(Data) orelse is_list(Data)) andalso is_map(Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data    the base-64 encoded data to decode
%% @returns the decoded data as a string
%% @doc     Equivalent to `decode_to_string(Data, #{})'.
%%
%%          Raises a `badarg' exception if the input is not valid
%%          base-64-encoded data.  Whitespace in the input is not accepted.
%% @end
%%-----------------------------------------------------------------------------
-spec decode_to_string(Data :: binary() | iolist()) -> string().
decode_to_string(Data) when is_binary(Data) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data    the base-64 encoded data to decode
%% @param   Options decoding options
%% @returns the decoded data as a string
%% @doc     Base-64 decode a binary or string, returning a string.
%%
%%          When `#{padding => true}' (default), the input must carry the
%%          correct number of `=' padding characters.  When
%%          `#{padding => false}', missing `=' padding is tolerated; padded
%%          input is still accepted.
%%
%%          Raises a `badarg' exception if the input is not valid
%%          base-64-encoded data.  Whitespace in the input is not accepted.
%%
%%          Note: the `mode' option is not supported; only the standard
%%          alphabet (RFC 4648 Section 4) is available.
%% @end
%%-----------------------------------------------------------------------------
-spec decode_to_string(Data :: binary() | iolist(), Options :: decode_options()) -> string().
decode_to_string(Data, Options) when is_binary(Data) andalso is_map(Options) ->
    erlang:nif_error(undefined).
