%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
%% @doc An implementation of a subset of the Erlang/OTP filename interface.
%%
%% This module implements a strict subset of the Erlang/OTP filename
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(filename).

-export([
    join/1,
    split/1
]).

%%-----------------------------------------------------------------------------
%% @param   Components list of path components to join
%% @returns the path formed by joining the components with "/"
%% @doc     Join a list of path components.
%%
%%          If a component is absolute (starts with "/"), all preceding
%%          components are discarded.  Redundant directory separators are
%%          removed from the result.
%% @end
%%-----------------------------------------------------------------------------
-spec join(Components :: [string()]) -> string().
join([]) ->
    error(function_clause);
join([Name]) ->
    normalize(Name);
join([Name | Rest]) ->
    join([do_join(Name, hd(Rest)) | tl(Rest)]).

%% @private
do_join(_Left, Right) when hd(Right) =:= $/ ->
    normalize(Right);
do_join(Left, Right) ->
    normalize(Left ++ "/" ++ Right).

%%-----------------------------------------------------------------------------
%% @param   Name a path to split into its components
%% @returns list of path components
%% @doc     Split a path into its components.
%%
%%          If the path is absolute, the first component is "/".
%%          Redundant directory separators are treated as a single separator.
%%          Trailing separators are ignored.
%% @end
%%-----------------------------------------------------------------------------
-spec split(Name :: string()) -> [string()].
split([]) ->
    [];
split([$/ | Rest]) ->
    ["/" | split_rel(skip_slashes(Rest))];
split(Name) ->
    split_rel(Name).

%% @private
%% Split a relative path (no leading slashes) into components.
split_rel([]) ->
    [];
split_rel(Name) ->
    {Component, Rest} = take_component(Name, []),
    case {Component, Rest} of
        {[], _} ->
            split_rel(skip_slashes(Rest));
        {C, []} ->
            [lists:reverse(C)];
        {C, [_ | Tail]} ->
            [lists:reverse(C) | split_rel(skip_slashes(Tail))]
    end.

%% @private
%% Collect characters up to the next "/" into a reversed accumulator.
take_component([], Acc) ->
    {Acc, []};
take_component([$/ | _] = Rest, Acc) ->
    {Acc, Rest};
take_component([C | Rest], Acc) ->
    take_component(Rest, [C | Acc]).

%% @private
%% Drop all leading "/" characters.
skip_slashes([$/ | Rest]) -> skip_slashes(Rest);
skip_slashes(Rest) -> Rest.

%% @private
%% Collapse every run of consecutive "/" into a single "/".
normalize([]) ->
    [];
normalize([$/, $/ | Rest]) ->
    normalize([$/ | Rest]);
normalize([C | Rest]) ->
    [C | normalize(Rest)].
