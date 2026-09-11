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
%% @doc Extract natively registered functions from AtomVM's gperf keyword files.
%%
%% Parses bifs.gperf and nifs.gperf and prints every registered function as
%%
%%     Module:Function/Arity<TAB>file:line
%%
%% sorted and unique, one line per function. The location column is what lets
%% check_native_stubs.erl point at the registration it is complaining about;
%% --names-only drops it.
%%
%% A gperf keyword line lives after the single standalone `%%' delimiter and the
%% registered name is the text before the first comma:
%%
%%     binary:at/2, &binary_at_nif
%%     file:get_cwd/0, IF_HAVE_GETCWD_PATHMAX(&file_get_cwd_nif)
%%     erlang:=/=/2, {.bif.base.type = BIFFunctionType, ...}
%%
%% Parsing the name is not a plain split on `/' or `:' because operator
%% functions (`+', `/', `=/=', `!', `++', ...) contain those characters in the
%% function name itself. The rule is: the module is the text before the FIRST
%% `:', the arity is the run of digits after the LAST `/', and the function name
%% is everything in between.
%%
%% Conditional registrations, whose value field is wrapped in an IF_HAVE_*()
%% macro, are ordinary registrations here: the function is part of the union of
%% what AtomVM can provide, and it needs an Erlang home either way.
%%
%% An unparseable keyword line aborts instead of warning. The format is strict,
%% and silently skipping a line we do not understand is how a registration
%% consistency check goes blind.
%%
%% Usage:
%%   extract_gperf_registrations.erl [-o FILE] [--names-only] FILE...
%% @end
%%-----------------------------------------------------------------------------

-mode(compile).

main([]) ->
    usage(),
    halt(1);
main(Args) ->
    Opts = parse_args(Args, #{out => stdout, names_only => false, files => []}),
    case maps:get(files, Opts) of
        [] ->
            usage(),
            halt(1);
        Files ->
            Registrations = lists:foldl(fun parse_file/2, [], Files),
            Lines = [format(Reg, Opts) || Reg <- group(lists:usort(Registrations))],
            output(Lines, Opts)
    end.

parse_args(["-o", File | Rest], Opts) ->
    parse_args(Rest, Opts#{out := File});
parse_args(["--names-only" | Rest], Opts) ->
    parse_args(Rest, Opts#{names_only := true});
parse_args(["--help" | _Rest], _Opts) ->
    usage(),
    halt(0);
parse_args([File | Rest], #{files := Files} = Opts) ->
    parse_args(Rest, Opts#{files := Files ++ [File]});
parse_args([], Opts) ->
    Opts.

parse_file(File, Acc) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Lines = numbered_lines(unicode:characters_to_list(Bin)),
            lists:foldl(
                fun(Line, InnerAcc) -> parse_line(Line, File, InnerAcc) end,
                Acc,
                keyword_section(Lines)
            );
        {error, Reason} ->
            abort("cannot read ~s: ~p", [File, Reason])
    end.

numbered_lines(Text) ->
    Lines = string:split(Text, "\n", all),
    lists:zip(lists:seq(1, length(Lines)), Lines).

keyword_section(Lines) ->
    case lists:splitwith(fun(Line) -> not is_delimiter(Line) end, Lines) of
        {_Declarations, [_Delimiter | Keywords]} ->
            lists:takewhile(fun(Line) -> not is_delimiter(Line) end, Keywords);
        {_Declarations, []} ->
            []
    end.

is_delimiter({_LineNo, Line}) ->
    string:trim(Line) =:= "%%".

parse_line({LineNo, Line}, File, Acc) ->
    case string:trim(Line) of
        "" ->
            Acc;
        Trimmed ->
            [Keyword | _Value] = string:split(Trimmed, ","),
            case parse_mfa(string:trim(Keyword)) of
                {ok, MFA} ->
                    Location = lists:flatten(io_lib:format("~s:~b", [File, LineNo])),
                    [{MFA, Location} | Acc];
                error ->
                    abort("~s:~b: unparseable keyword line: ~ts", [File, LineNo, Trimmed])
            end
    end.

parse_mfa(Keyword) ->
    case string:split(Keyword, ":") of
        [Module, Rest] when Module =/= [], Rest =/= [] ->
            case split_last_slash(Rest) of
                {Function, Arity} when Function =/= [] ->
                    case is_all_digits(Arity) of
                        true -> {ok, Module ++ ":" ++ Function ++ "/" ++ Arity};
                        false -> error
                    end;
                error ->
                    error
            end;
        _Other ->
            error
    end.

split_last_slash(Str) ->
    case string:find(Str, "/", trailing) of
        nomatch ->
            error;
        SlashAndArity ->
            FunctionLen = length(Str) - length(SlashAndArity),
            {lists:sublist(Str, FunctionLen), tl(SlashAndArity)}
    end.

is_all_digits([]) ->
    false;
is_all_digits(Str) ->
    lists:all(fun(C) -> C >= $0 andalso C =< $9 end, Str).

group([]) ->
    [];
group([{MFA, Location} | Rest]) ->
    {Same, Others} = lists:splitwith(fun({Other, _}) -> Other =:= MFA end, Rest),
    [{MFA, [Location | [L || {_, L} <- Same]]} | group(Others)].

format({MFA, _Locations}, #{names_only := true}) ->
    MFA;
format({MFA, Locations}, _Opts) ->
    [MFA, $\t, lists:join(",", Locations)].

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
        "usage: extract_gperf_registrations.erl [-o FILE] [--names-only] FILE...~n"
        "~n"
        "  FILE is a gperf keyword file (e.g. bifs.gperf, nifs.gperf).~n"
        "  Prints sorted unique \"Module:Function/Arity<TAB>file:line\" lines.~n"
        "  --names-only drops the location column.~n",
        []
    ).
