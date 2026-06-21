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
    basename/1,
    basename/2,
    dirname/1,
    extension/1,
    join/1,
    join/2,
    rootname/1,
    rootname/2,
    split/1
]).

%%-----------------------------------------------------------------------------
%% @param   Components list of path components to join
%% @returns the path formed by joining the components with "/"
%% @doc     Join a list of path components.
%%
%%          If a component is absolute (starts with "/"), all preceding
%%          components are discarded.  Redundant directory separators and
%%          "." components followed by a separator are removed from the
%%          result.
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
do_join(Left, []) ->
    normalize(Left);
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
normalize(Name) ->
    strip_join_trailing_slash(collapse_separators(Name)).

%% @private
%% Collapse every run of consecutive "/" into a single "/", and drop any
%% "." component followed by a separator ("/./" -> "/"), as OTP join does.
%% A leading "./" and a trailing "/." are kept.
collapse_separators([]) ->
    [];
collapse_separators([$/, $/ | Rest]) ->
    collapse_separators([$/ | Rest]);
collapse_separators([$/, $., $/ | Rest]) ->
    collapse_separators([$/ | Rest]);
collapse_separators([C | Rest]) ->
    [C | collapse_separators(Rest)].

%% @private
%% Remove a single trailing "/", except for the root "/" itself.
strip_join_trailing_slash([$/]) -> [$/];
strip_join_trailing_slash(Name) -> strip_trailing_slash(Name).

%% @private
%% Remove a single trailing "/", if present.
strip_trailing_slash([]) -> [];
strip_trailing_slash([$/]) -> [];
strip_trailing_slash([C | Rest]) -> [C | strip_trailing_slash(Rest)].

%%-----------------------------------------------------------------------------
%% @param   Name1 first path component
%% @param   Name2 second path component
%% @returns the path formed by joining Name1 and Name2
%% @doc     Join two path components, equivalent to `join([Name1, Name2])'.
%% @end
%%-----------------------------------------------------------------------------
-spec join(Name1 :: string(), Name2 :: string()) -> string().
join(Name1, []) ->
    join([Name1]);
join(Name1, Name2) ->
    join([Name1, Name2]).

%%-----------------------------------------------------------------------------
%% @param   Name a file name
%% @returns the directory part of Name
%% @doc     Return the directory part of a file name, "." if there is none.
%% @end
%%-----------------------------------------------------------------------------
-spec dirname(Name :: string()) -> string().
dirname(Name) ->
    %% Drop the last component (the characters after the last slash, which
    %% may be empty for names with a trailing slash), then the separating
    %% slash run; what remains, reversed, is the directory.
    case drop_component(lists:reverse(Name)) of
        no_slash ->
            ".";
        [] ->
            "/";
        DirReversed ->
            lists:reverse(DirReversed)
    end.

%% @private
%% Drop the reversed last component and the slash run separating it; return
%% the reversed directory part, or no_slash if the name has no slash at all.
drop_component([]) -> no_slash;
drop_component([$/ | _] = Rest) -> skip_slashes(Rest);
drop_component([_ | Rest]) -> drop_component(Rest).

%%-----------------------------------------------------------------------------
%% @param   Name a file name
%% @returns the last component of Name
%% @doc     Return the last component of a file name, ignoring a single
%%          trailing slash (further ones make the last component empty).
%% @end
%%-----------------------------------------------------------------------------
-spec basename(Name :: string()) -> string().
basename(Name) ->
    basename(Name, []).

%%-----------------------------------------------------------------------------
%% @param   Name a file name
%% @param   Ext an extension
%% @returns the last component of Name, with Ext stripped if it matches
%% @doc     Like `basename/1', but also removes the extension Ext when the
%%          base name ends with it.
%% @end
%%-----------------------------------------------------------------------------
-spec basename(Name :: string(), Ext :: string()) -> string().
basename(Name, Ext) ->
    basename(Name, Ext, []).

%% @private
%% Scan Name, accumulating the current component (reversed) in Tail and
%% resetting it at each "/"; when the remainder equals Ext, the component
%% accumulated so far is the base name.  A single trailing "/" is ignored.
%% This mirrors OTP basename/2; basename/1 is the Ext = "" case.
basename(Ext, Ext, Tail) ->
    lists:reverse(Tail);
basename([$/], Ext, Tail) ->
    basename([], Ext, Tail);
basename([$/ | Rest], Ext, _Tail) ->
    basename(Rest, Ext, []);
basename([C | Rest], Ext, Tail) ->
    basename(Rest, Ext, [C | Tail]);
basename([], _Ext, Tail) ->
    lists:reverse(Tail).

%%-----------------------------------------------------------------------------
%% @param   Name a file name
%% @returns the extension of Name, including the dot, or "" if there is none
%% @doc     Return the file extension of the last path component, "" if the
%%          last component has no dot.
%% @end
%%-----------------------------------------------------------------------------
-spec extension(Name :: string()) -> string().
extension(Name) ->
    case extension_split(Name) of
        {_Root, Ext} -> Ext
    end.

%%-----------------------------------------------------------------------------
%% @param   Name a file name
%% @returns Name with its extension removed
%% @doc     Remove the extension of the last path component, if any.
%% @end
%%-----------------------------------------------------------------------------
-spec rootname(Name :: string()) -> string().
rootname(Name) ->
    case leading_dot_only(Name) of
        true ->
            [];
        false ->
            case extension_split(Name) of
                {Root, _Ext} -> Root
            end
    end.

%% @private
%% True if Name is a leading-dot dotfile with no directory and no further
%% dot (e.g. ".bashrc"), which OTP treats as having no root name.
leading_dot_only([$. | Rest]) ->
    not lists:member($/, Rest) andalso not lists:member($., Rest);
leading_dot_only(_) ->
    false.

%%-----------------------------------------------------------------------------
%% @param   Name a file name
%% @param   Ext an extension
%% @returns Name with Ext removed if Name ends with Ext
%% @doc     Remove Ext from Name when it is its extension.
%% @end
%%-----------------------------------------------------------------------------
-spec rootname(Name :: string(), Ext :: string()) -> string().
rootname(Name, Ext) ->
    strip_root_suffix(Name, Ext, [], Name).

%% @private
%% Remove Ext from the end of Name if present, except when the character
%% before it is a "/" (i.e. Ext is the whole last component), matching
%% OTP rootname/2.
strip_root_suffix(Ext, Ext, [$/ | _], Name) -> Name;
strip_root_suffix(Ext, Ext, Acc, _Name) -> lists:reverse(Acc);
strip_root_suffix([], _Ext, _Acc, Name) -> Name;
strip_root_suffix([C | Rest], Ext, Acc, Name) -> strip_root_suffix(Rest, Ext, [C | Acc], Name).

%% @private
%% Split Name into {Root, Extension}: the extension starts at the last "." of
%% the last path component, unless that dot is the component's first character
%% (hidden files such as ".bashrc" have no extension).
extension_split(Name) ->
    case extension_length(lists:reverse(Name), 0) of
        0 ->
            {Name, []};
        ExtLen ->
            NameLen = length(Name),
            {lists:sublist(Name, NameLen - ExtLen), lists:nthtail(NameLen - ExtLen, Name)}
    end.

%% @private
%% Scan the reversed name for the dot starting the extension; return the
%% extension length (including the dot), or 0 if there is none.
extension_length([], _Consumed) ->
    0;
extension_length([$/ | _], _Consumed) ->
    0;
extension_length([$., Next | _], Consumed) when Next =/= $/ ->
    Consumed + 1;
extension_length([$.], _Consumed) ->
    0;
extension_length([$., $/ | _], _Consumed) ->
    0;
extension_length([_ | Rest], Consumed) ->
    extension_length(Rest, Consumed + 1).
