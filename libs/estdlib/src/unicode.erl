%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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
%% @doc An implementation of the Erlang/OTP unicode interface.
%%
%% This module implements a strict subset of the Erlang/OTP unicode
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(unicode).

-export([
    characters_to_list/1,
    characters_to_list/2,
    characters_to_binary/1,
    characters_to_binary/2,
    characters_to_binary/3
]).

%% A UTF-8 encoded binary.
-type unicode_binary() :: binary().

%% Latin-1 encoded data
-type latin1_chardata() :: iodata().

%% Unicode or UTF-8 encoded data
-type chardata() :: charlist() | unicode_binary().
-type charlist() :: maybe_improper_list(
    char() | unicode_binary() | charlist(), unicode_binary() | []
).

-type encoding() :: utf8 | unicode | latin1.

-export_type([
    unicode_binary/0,
    latin1_chardata/0,
    chardata/0,
    charlist/0,
    encoding/0
]).

%% @doc Convert UTF-8 data to a list of Unicode characters.
%% <p>If conversion fails, the function returns a tuple with three elements:</p>
%% <ul>
%%     <li>First element is <code>error</code> or <code>incomplete</code>. <code>incomplete</code> means the conversion failed because of an incomplete unicode transform at the very end of data.</li>
%%     <li>Second element is what has been converted so far.</li>
%%     <li>Third element is the remaining data to be converted, for debugging purposes. This remaining data can differ with what Erlang/OTP returns.</li>
%% </ul>
%% @param Data data to convert to Unicode
%% @return a list of characters or a tuple if conversion failed.
-spec characters_to_list(Data :: chardata() | latin1_chardata()) ->
    list()
    | {error, list(), chardata() | latin1_chardata() | list()}
    | {incomplete, list(), chardata() | latin1_chardata()}.
characters_to_list(_Data) ->
    erlang:nif_error(undefined).

%% @doc Convert UTF-8 or Latin1 data to a list of Unicode characters.
%% Following Erlang/OTP, if input encoding is latin1, this function returns
%% an error tuple if a character > 255 is passed (in a list). Otherwise, it
%% will accept any character within Unicode range (0-0x10FFFF).
%% @see characters_to_list/1
%% @param Data data to convert
%% @param Encoding encoding of data to convert
%% @return a list of characters or a tuple if conversion failed.
-spec characters_to_list(Data :: chardata() | latin1_chardata(), Encoding :: encoding()) ->
    list()
    | {error, list(), chardata() | latin1_chardata() | list()}
    | {incomplete, list(), chardata() | latin1_chardata()}.
characters_to_list(_Data, _Encoding) ->
    erlang:nif_error(undefined).

%% @doc Convert character data to an UTF8 binary
%% @equiv characters_to_binary(Data, utf8, utf8)
%% @param Data data to convert to UTF8
%% @return an utf8 binary or a tuple if conversion failed.
-spec characters_to_binary(Data :: chardata() | latin1_chardata()) ->
    unicode_binary()
    | {error, list(), chardata() | latin1_chardata() | list()}
    | {incomplete, unicode_binary(), chardata() | latin1_chardata()}.
characters_to_binary(_Data) ->
    erlang:nif_error(undefined).

%% @doc Convert character data in a given encoding to an UTF8 binary
%% @equiv characters_to_binary(Data, InEncoding, utf8)
%% @param Data data to convert to UTF8
%% @param InEncoding encoding of data
%% @return an utf8 binary or a tuple if conversion failed.
-spec characters_to_binary(Data :: chardata() | latin1_chardata(), InEncoding :: encoding()) ->
    unicode_binary()
    | {error, list(), chardata() | latin1_chardata() | list()}
    | {incomplete, unicode_binary(), chardata() | latin1_chardata()}.
characters_to_binary(_Data, _InEncoding) ->
    erlang:nif_error(undefined).

%% @doc Convert character data in a given encoding to a binary in a given encoding.
%% <p>If conversion fails, the function returns a tuple with three elements:</p>
%% <ul>
%%     <li>First element is <code>error</code> or <code>incomplete</code>. <code>incomplete</code> means the conversion failed because of an incomplete unicode transform at the very end of data.</li>
%%     <li>Second element is what has been converted so far.</li>
%%     <li>Third element is the remaining data to be converted, for debugging purposes. This remaining data can differ with what Erlang/OTP returns.</li>
%% </ul>
%% <p>Also, Erlang/OTP's implementation may error with <code>badarg</code> for parameters
%% for which this function merely returns an error tuple.</p>
%% @param Data data to convert to UTF8
%% @param InEncoding encoding of input data
%% @param OutEncoding output encoding
%% @return an encoded binary or a tuple if conversion failed.
-spec characters_to_binary(
    Data :: chardata() | latin1_chardata(), InEncoding :: encoding(), OutEncoding :: encoding()
) ->
    unicode_binary()
    | {error, list(), chardata() | latin1_chardata() | list()}
    | {incomplete, unicode_binary(), chardata() | latin1_chardata()}.
characters_to_binary(_Data, _InEncoding, _OutEncoding) ->
    erlang:nif_error(undefined).
