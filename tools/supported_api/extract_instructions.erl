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
%% @doc Extract the supported BEAM instructions from opcodes.def.
%%
%% Prints the name of every opcode the VM implements, one per line, in opcode
%% order. opcodes.def is an X macro table with one invocation per line and four
%% macros, of which only two describe an implemented opcode:
%%
%%     X_OPCODE(OP_FUNC_INFO, 2, func_info, "aaA")       implemented
%%     X_OPCODE_HANDLER(OP_LABEL, 1, label, "I")         implemented, own handler
%%     X_OPCODE_REMOVED(OP_PUT_TUPLE, 70, put_tuple)     gone from the BEAM
%%     X_OPCODE_SKIP(27)                                 never existed
%%
%% The name is the third argument, which is also what the VM stringizes for its
%% own opcode name table.
%%
%% Anything else in the file is an error: an X macro this script does not know, a
%% known macro with the wrong number of arguments, a name that is not a bare
%% lowercase atom, or a line that is neither blank, a comment, nor an invocation.
%% Refusing to guess is the point. The alternative to an error here is a silently
%% incomplete or over-stated list of what the VM can run.
%%
%% Note that the list reflects opcodes.def, not the configured build: a handful of
%% opcodes have their case arms compiled out by SUPPORT_COMPILER_OPT_* or by
%% MAXIMUM_OTP_COMPILER_VERSION in opcodesswitch.h, and that is not visible here.
%% The default configuration implements all of them.
%%
%% Usage:
%%   extract_instructions.erl [-o FILE] FILE...
%% @end
%%-----------------------------------------------------------------------------

-mode(compile).

main([]) ->
    usage(),
    halt(1);
main(Args) ->
    Opts = parse_args(Args, #{out => stdout, files => []}),
    case maps:get(files, Opts) of
        [] ->
            usage(),
            halt(1);
        Files ->
            output(lists:flatmap(fun instructions/1, Files), Opts)
    end.

parse_args(["-o", File | Rest], Opts) ->
    parse_args(Rest, Opts#{out := File});
parse_args(["--help" | _Rest], _Opts) ->
    usage(),
    halt(0);
parse_args([File | Rest], #{files := Files} = Opts) ->
    parse_args(Rest, Opts#{files := Files ++ [File]});
parse_args([], Opts) ->
    Opts.

instructions(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Lines = string:split(unicode:characters_to_list(Bin), "\n", all),
            {Instructions, _InComment} = lists:foldl(
                fun(Line, Acc) -> parse_line(Line, File, Acc) end,
                {[], false},
                lists:zip(lists:seq(1, length(Lines)), Lines)
            ),
            lists:reverse(Instructions);
        {error, Reason} ->
            abort("cannot read ~s: ~p", [File, Reason])
    end.

parse_line({LineNo, Line}, File, {Acc, InComment0}) ->
    {Code, InComment} = strip_comments(Line, InComment0),
    {parse_code(string:trim(Code), File, LineNo, Acc), InComment}.

strip_comments(Line, true) ->
    case string:split(Line, "*/") of
        [_Comment] -> {"", true};
        [_Comment, Rest] -> strip_comments(Rest, false)
    end;
strip_comments(Line, false) ->
    case {string:find(Line, "/*"), string:find(Line, "//")} of
        {nomatch, nomatch} ->
            {Line, false};
        {nomatch, LineComment} ->
            {before(Line, LineComment), false};
        {BlockComment, nomatch} ->
            strip_block_comment(Line, BlockComment);
        {BlockComment, LineComment} when length(BlockComment) > length(LineComment) ->
            strip_block_comment(Line, BlockComment);
        {_BlockComment, LineComment} ->
            {before(Line, LineComment), false}
    end.

strip_block_comment(Line, BlockComment) ->
    "/*" ++ AfterOpen = BlockComment,
    {Rest, InComment} = strip_comments(AfterOpen, true),
    {before(Line, BlockComment) ++ Rest, InComment}.

before(Line, Tail) ->
    lists:sublist(Line, length(Line) - length(Tail)).

parse_code("", _File, _LineNo, Acc) ->
    Acc;
parse_code(Code, File, LineNo, Acc) ->
    case invocation(Code) of
        {"X_OPCODE", [_OpName, _Number, Name, _Signature]} ->
            [instruction_name(Name, Code, File, LineNo) | Acc];
        {"X_OPCODE_HANDLER", [_OpName, _Number, Name, _Signature]} ->
            [instruction_name(Name, Code, File, LineNo) | Acc];
        {"X_OPCODE_REMOVED", [_OpName, _Number, _Name]} ->
            Acc;
        {"X_OPCODE_SKIP", [_Number]} ->
            Acc;
        {Macro, Arguments} ->
            abort("~s:~b: unexpected ~s with ~b argument(s): ~ts", [
                File, LineNo, Macro, length(Arguments), Code
            ]);
        error ->
            abort("~s:~b: not an X macro invocation: ~ts", [File, LineNo, Code])
    end.

invocation(Code) ->
    case string:split(Code, "(") of
        [Macro, Rest] ->
            case lists:reverse(Rest) of
                ")" ++ ReversedArguments ->
                    Arguments = string:split(lists:reverse(ReversedArguments), ",", all),
                    {string:trim(Macro), [string:trim(A) || A <- Arguments]};
                _NoClosingParenthesis ->
                    error
            end;
        [_NoParenthesis] ->
            error
    end.

instruction_name(Name, Code, File, LineNo) ->
    case is_instruction_name(Name) of
        true -> Name;
        false -> abort("~s:~b: not an instruction name: ~ts", [File, LineNo, Code])
    end.

is_instruction_name([First | Rest]) when First >= $a, First =< $z ->
    lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_
        end,
        Rest
    );
is_instruction_name(_Name) ->
    false.

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
        "usage: extract_instructions.erl [-o FILE] FILE...~n"
        "~n"
        "  FILE is an X macro opcode table (src/libAtomVM/opcodes.def).~n"
        "  Prints the name of every implemented opcode, in opcode order.~n",
        []
    ).
