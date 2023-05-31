%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Fred Dushin <fred@dushin.net>
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
%% @doc An implementation of the Erlang/OTP io_lib interface.
%%
%% This module implements a strict subset of the Erlang/OTP io_lib
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(io_lib).

-export([format/2]).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format argument
%% @returns string
%% @doc     Format string and data to a string.
%%          Approximates features of OTP io_lib:format/2, but
%%          only supports ~p and ~n format specifiers.
%%          Raises `badarg' error if the number of format specifiers
%%          does not match the length of the Args.
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format :: string(), Args :: list()) -> string().
format(Format, Args) ->
    {FormatTokens, Instr} = split(Format),
    case length(FormatTokens) == length(Args) + 1 of
        true ->
            interleave(FormatTokens, Instr, Args, []);
        false ->
            throw(badarg)
    end.

%%
%% internal operations
%%

%% @private
split(Format) ->
    split(Format, [], [], []).

%% @private
split([], Cur, Accum, Instr) ->
    {lists:reverse([lists:reverse(Cur) | Accum]), lists:reverse(Instr)};
split([$~, $s | Rest], Cur, Accum, Instr) ->
    split(Rest, [], [lists:reverse(Cur) | Accum], [fun(T) -> format_spw(s, T) end | Instr]);
split([$~, $p | Rest], Cur, Accum, Instr) ->
    split(Rest, [], [lists:reverse(Cur) | Accum], [fun(T) -> format_spw(p, T) end | Instr]);
split([$~, $w | Rest], Cur, Accum, Instr) ->
    split(Rest, [], [lists:reverse(Cur) | Accum], [fun(T) -> format_spw(w, T) end | Instr]);
split([$~, $n | Rest], Cur, Accum, Instr) ->
    split(Rest, [$\n | Cur], Accum, Instr);
split([$~, $~ | Rest], Cur, Accum, Instr) ->
    split(Rest, [$~ | Cur], Accum, Instr);
split([Char | Rest], Cur, Accum, Instr) ->
    split(Rest, [Char | Cur], Accum, Instr).

%% @private
interleave([LastToken], _Instr, [], Accum) ->
    lists:reverse([LastToken | Accum]);
interleave([Token | Tokens], [Formatter | Instr], [Arg | Args], Accum) ->
    interleave(Tokens, Instr, Args, [Formatter(Arg), Token | Accum]).

%% @private
format_spw(_Mode, T) when is_atom(T) ->
    erlang:atom_to_list(T);
format_spw(s, T) when is_binary(T) ->
    erlang:binary_to_list(T);
format_spw(Mode, T) when is_binary(T) ->
    L = erlang:binary_to_list(T),
    FormattedStr =
        case {Mode, test_string_class(L, printable)} of
            {p, printable} -> format_p_string(L, []);
            _ -> lists:join($,, [integer_to_list(B) || B <- L])
        end,
    [$<, $<, FormattedStr, $>, $>];
format_spw(Mode, L) when is_list(L) ->
    case {Mode, test_string_class(L, printable)} of
        {s, not_a_string} -> throw(badarg);
        {s, _} -> L;
        {p, printable} -> format_p_string(L, []);
        _ -> [$[, lists:join($,, [format_spw(Mode, E) || E <- L]), $]]
    end;
format_spw(s, _) ->
    throw(badarg);
format_spw(_Mode, T) when is_integer(T) ->
    erlang:integer_to_list(T);
format_spw(_Mode, T) when is_float(T) ->
    erlang:float_to_list(T);
format_spw(_Mode, T) when is_pid(T) ->
    erlang:pid_to_list(T);
format_spw(_Mode, T) when is_reference(T) ->
    erlang:ref_to_list(T);
format_spw(_Mode, T) when is_function(T) ->
    erlang:fun_to_list(T);
format_spw(Mode, T) when is_tuple(T) ->
    [${, lists:join($,, [format_spw(Mode, E) || E <- tuple_to_list(T)]), $}];
format_spw(Mode, T) when is_map(T) ->
    [
        $#,
        ${,
        lists:join($,, [
            [format_spw(Mode, K), " => ", format_spw(Mode, V)]
         || {K, V} <- maps:to_list(T)
        ]),
        $}
    ].

%% @private
test_string_class([H | T], _Class) when is_integer(H) andalso H >= 0 andalso H < 8 ->
    test_string_class(T, unprintable);
test_string_class([H | T], _Class) when is_integer(H) andalso H >= 14 andalso H < 27 ->
    test_string_class(T, unprintable);
test_string_class([H | T], _Class) when is_integer(H) andalso H >= 28 andalso H < 32 ->
    test_string_class(T, unprintable);
test_string_class([H | T], _Class) when is_integer(H) andalso H >= 127 andalso H < 160 ->
    test_string_class(T, unprintable);
test_string_class([H | T], Class) when is_integer(H) andalso H >= 8 andalso H < 256 ->
    test_string_class(T, Class);
test_string_class([H | T], _Class) when is_list(H) ->
    case test_string_class(H, iolist) of
        iolist -> test_string_class(T, iolist);
        Other -> Other
    end;
test_string_class([], Class) ->
    Class;
test_string_class(_, _Class) ->
    not_a_string.

%% @private
format_p_string([], Acc) ->
    [$", lists:reverse(Acc), $"];
format_p_string([8 | T], Acc) ->
    format_p_string(T, ["\\b" | Acc]);
format_p_string([9 | T], Acc) ->
    format_p_string(T, ["\\t" | Acc]);
format_p_string([10 | T], Acc) ->
    format_p_string(T, ["\\n" | Acc]);
format_p_string([11 | T], Acc) ->
    format_p_string(T, ["\\v" | Acc]);
format_p_string([12 | T], Acc) ->
    format_p_string(T, ["\\f" | Acc]);
format_p_string([13 | T], Acc) ->
    format_p_string(T, ["\\r" | Acc]);
format_p_string([27 | T], Acc) ->
    format_p_string(T, ["\\e" | Acc]);
format_p_string([H | T], Acc) ->
    format_p_string(T, [H | Acc]).
