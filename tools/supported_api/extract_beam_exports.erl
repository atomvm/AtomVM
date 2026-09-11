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
%% @doc Extract exported functions from compiled BEAM files.
%%
%% Prints every exported function of the given beams as
%%
%%     Module:Function/Arity
%%
%% sorted and unique. The export chunk is the single source of truth for what is
%% callable on a given AtomVM build: it already reflects conditional compilation,
%% so the union over the beams of a library is that library's contribution to
%% funcs.txt with no merge logic.
%%
%% module_info/0 and module_info/1 are included: they are callable, and funcs.txt
%% lists them. Elixir modules keep their `Elixir.' prefix, and operator functions
%% keep their raw spelling (`+', `=:=', ...), which is how they are spelled in the
%% gperf tables as well.
%%
%% A file that cannot be read as a beam is reported on standard error and
%% skipped rather than fatal: the input is usually the content listing of an
%% archive, which may name something that is not a beam. A library whose exports
%% go missing this way shows up either as a gate failure or as a missing entry in
%% the funcs.txt sanity checks.
%%
%% Usage:
%%   extract_beam_exports.erl [-o FILE] PATH...
%%   extract_beam_exports.erl [-o FILE] --files-from FILE
%%
%% A PATH is a .beam file or a directory, searched recursively. --files-from
%% reads the list of beams from a file, one path per line, which is how the build
%% hands over exactly the modules an .avm archive ships.
%% @end
%%-----------------------------------------------------------------------------

-mode(compile).

main([]) ->
    usage(),
    halt(1);
main(Args) ->
    Opts = parse_args(Args, #{out => stdout, paths => []}),
    case maps:get(paths, Opts) of
        [] ->
            usage(),
            halt(1);
        Paths ->
            Beams = lists:flatmap(fun find_beams/1, Paths),
            Exports = lists:foldl(fun collect_exports/2, [], Beams),
            output(lists:usort(Exports), Opts)
    end.

parse_args(["-o", File | Rest], Opts) ->
    parse_args(Rest, Opts#{out := File});
parse_args(["--files-from", File | Rest], #{paths := Paths} = Opts) ->
    parse_args(Rest, Opts#{paths := Paths ++ read_list(File)});
parse_args(["--help" | _Rest], _Opts) ->
    usage(),
    halt(0);
parse_args([Path | Rest], #{paths := Paths} = Opts) ->
    parse_args(Rest, Opts#{paths := Paths ++ [Path]});
parse_args([], Opts) ->
    Opts.

read_list(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Lines = string:split(unicode:characters_to_list(Bin), "\n", all),
            [Line || Line <- [string:trim(L) || L <- Lines], Line =/= ""];
        {error, Reason} ->
            abort("cannot read ~s: ~p", [File, Reason])
    end.

find_beams(Path) ->
    case filelib:is_dir(Path) of
        true ->
            filelib:fold_files(Path, ".*\\.beam$", true, fun(F, Acc) -> [F | Acc] end, []);
        false ->
            [Path]
    end.

collect_exports(Beam, Acc) ->
    case beam_lib:chunks(Beam, [exports]) of
        {ok, {Module, [{exports, Exports}]}} ->
            [format(Module, Function, Arity) || {Function, Arity} <- Exports] ++ Acc;
        {error, beam_lib, Reason} ->
            io:format(standard_error, "warning: cannot read ~s: ~p~n", [Beam, Reason]),
            Acc
    end.

format(Module, Function, Arity) ->
    lists:flatten(io_lib:format("~ts:~ts/~b", [Module, Function, Arity])).

output(Lines, Opts) ->
    Text = [[Line, $\n] || Line <- Lines],
    case maps:get(out, Opts) of
        stdout ->
            io:put_chars(Text);
        File ->
            case file:write_file(File, Text) of
                ok ->
                    ok;
                {error, Reason} ->
                    abort("cannot write ~s: ~p", [File, Reason])
            end
    end.

abort(Format, Args) ->
    io:format(standard_error, "error: " ++ Format ++ "~n", Args),
    halt(1).

usage() ->
    io:format(
        standard_error,
        "usage: extract_beam_exports.erl [-o FILE] PATH...~n"
        "       extract_beam_exports.erl [-o FILE] --files-from FILE~n"
        "~n"
        "  PATH is a .beam file or a directory searched recursively.~n"
        "  --files-from reads beam paths from FILE, one per line.~n"
        "  Prints sorted unique Module:Function/Arity lines, module_info included.~n",
        []
    ).
