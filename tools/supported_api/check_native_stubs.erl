#!/usr/bin/env escript
%% -*- erlang -*-
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
%% @doc Check that every natively registered function has an Erlang export.
%%
%% Reads the registration list produced by extract_gperf_registrations.erl and
%% the per library export lists produced by extract_beam_exports.erl, and fails
%% when a registered function is exported by none of the built libraries.
%%
%% A registration without an export is a function with no home: no
%% documentation, no spec, nothing for Dialyzer or ExDoc to see, and no entry in
%% the generated funcs.txt, which is what build tools check projects against.
%% Such a function is still callable, because a NIF is looked up while a module's
%% import table is built, which is exactly why the omission goes unnoticed
%% without this check.
%%
%% Usage:
%%   check_native_stubs.erl --registrations FILE FRAGMENT...
%% @end
%%-----------------------------------------------------------------------------

-mode(compile).

main([]) ->
    usage(),
    halt(1);
main(Args) ->
    Opts = parse_args(Args, #{registrations => undefined, fragments => []}),
    Registrations = registrations(maps:get(registrations, Opts)),
    Exports = exports(maps:get(fragments, Opts)),
    case missing(Registrations, Exports) of
        [] ->
            io:format("check-native-stubs: ~s, all exported~n", [
                count(length(Registrations), "registration")
            ]),
            halt(0);
        Missing ->
            report(Missing),
            halt(1)
    end.

missing(Registrations, Exports) ->
    [Reg || {MFA, _Locations} = Reg <- Registrations, not maps:is_key(MFA, Exports)].

parse_args(["--registrations", File | Rest], Opts) ->
    parse_args(Rest, Opts#{registrations := File});
parse_args(["--help" | _Rest], _Opts) ->
    usage(),
    halt(0);
parse_args([Fragment | Rest], #{fragments := Fragments} = Opts) ->
    parse_args(Rest, Opts#{fragments := Fragments ++ [Fragment]});
parse_args([], #{registrations := undefined}) ->
    usage(),
    halt(1);
parse_args([], #{fragments := []}) ->
    usage(),
    halt(1);
parse_args([], Opts) ->
    Opts.

registrations(File) ->
    [parse_registration(Line) || Line <- lines(File)].

parse_registration(Line) ->
    case string:split(Line, "\t") of
        [MFA] -> {MFA, []};
        [MFA, Locations] -> {MFA, string:split(Locations, ",", all)}
    end.

exports(Fragments) ->
    maps:from_keys(lists:append([lines(Fragment) || Fragment <- Fragments]), true).

lines(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Lines = string:split(unicode:characters_to_list(Bin), "\n", all),
            [Line || Line <- [string:trim(L) || L <- Lines], Line =/= ""];
        {error, Reason} ->
            io:format(standard_error, "error: cannot read ~s: ~p~n", [File, Reason]),
            halt(1)
    end.

report(Missing) ->
    [{FirstMFA, _} | _] = Missing,
    io:format(standard_error, "check-native-stubs: ~s no Erlang export~n~n", [
        case length(Missing) of
            1 -> "1 native registration has";
            N -> io_lib:format("~b native registrations have", [N])
        end
    ]),
    lists:foreach(
        fun({MFA, Locations}) ->
            io:format(standard_error, "  ~ts~n", [MFA]),
            [io:format(standard_error, "      registered at ~ts~n", [L]) || L <- Locations]
        end,
        Missing
    ),
    io:format(standard_error, "~n~ts", [explanation(FirstMFA)]).

explanation(MFA) ->
    {Function, Arity} = function_and_arity(MFA),
    Args = lists:join(", ", ["_Arg" ++ integer_to_list(N) || N <- lists:seq(1, Arity)]),
    Types = lists:join(", ", ["term()" || _ <- lists:seq(1, Arity)]),
    io_lib:format(
        "Every function registered in bifs.gperf or nifs.gperf must have a matching~n"
        "export in the built AtomVM libraries: that export is where its documentation~n"
        "and its -spec live, and it is what makes the generated funcs.txt complete.~n"
        "~n"
        "Add a stub to the module that owns the function:~n"
        "~n"
        "    -export([~ts/~b]).~n"
        "~n"
        "    -spec ~ts(~ts) -> term().~n"
        "    ~ts(~ts) ->~n"
        "        erlang:nif_error(undefined).~n"
        "~n"
        "The explicit erlang: prefix is required: a function is resolved as a BIF, then~n"
        "as a NIF, then as a beam function when the import table is built, so the body~n"
        "of a stub only ever runs on a local call and has to raise instead of looping.~n"
        "~n"
        "If the module does not exist yet, add it to ERLANG_MODULES (or ELIXIR_MODULES)~n"
        "in its library's CMakeLists.txt as well, otherwise it is never compiled nor~n"
        "packed.~n",
        [Function, Arity, Function, Types, Function, Args]
    ).

function_and_arity(MFA) ->
    [_Module, Rest] = string:split(MFA, ":"),
    Reversed = lists:reverse(Rest),
    [ReversedArity, ReversedFunction] = string:split(Reversed, "/"),
    {quote(lists:reverse(ReversedFunction)), list_to_integer(lists:reverse(ReversedArity))}.

quote([First | Rest] = Function) when First >= $a, First =< $z ->
    case lists:all(fun is_atom_char/1, Rest) of
        true -> Function;
        false -> [$' | Function] ++ [$']
    end;
quote(Function) ->
    [$' | Function] ++ [$'].

is_atom_char(C) ->
    (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse C =:= $_ orelse C =:= $@.

count(1, Noun) ->
    "1 " ++ Noun;
count(N, Noun) ->
    io_lib:format("~b ~ss", [N, Noun]).

usage() ->
    io:format(
        standard_error,
        "usage: check_native_stubs.erl --registrations FILE FRAGMENT...~n"
        "~n"
        "  FILE is the output of extract_gperf_registrations.erl.~n"
        "  FRAGMENT is a per library export list, as produced by~n"
        "  extract_beam_exports.erl; pass one for every built library.~n",
        []
    ).
