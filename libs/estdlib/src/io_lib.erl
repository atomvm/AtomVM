%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(io_lib).

-export([format/2]).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format argument
%% @returns string
%% @doc     Format string and data to a string.
%%          Approximates features of OTP io_lib:format/2, but
%%          only supports ~p and ~n format specifiers.
%%          Raises bad_format error if the number of format specifiers
%%          does not match the length of the Args.
%% @end
%%-----------------------------------------------------------------------------
-spec format(Format::string(), Args::list()) -> string().
format(Format, Args) ->
    {FormatTokens, Instr} = split(Format),
    case length(FormatTokens) == length(Args) + 1 of
        true ->
            StringList = interleave(FormatTokens, Instr, Args, []),
            lists:flatten(StringList);
        false ->
            throw(bad_format)
    end.

%%
%% internal operations
%%

%% @private
split(Format) ->
    split(Format, [], [], []).

%% @private
split([], Cur, Accum, Instr) ->
    {lists:reverse([lists:reverse(Cur)|Accum]), lists:reverse(Instr)};
split([$~, $p | Rest], Cur, Accum, Instr) ->
    split(Rest, [], [lists:reverse(Cur)|Accum], [quote|Instr]);
split([$~, $s | Rest], Cur, Accum, Instr) ->
    split(Rest, [], [lists:reverse(Cur)|Accum], [literal|Instr]);
split([$~, $n | Rest], Cur, Accum, Instr) ->
    split(Rest, [$\n|Cur], Accum, Instr);
split([$~, $~ | Rest], Cur, Accum, Instr) ->
    split(Rest, [$~|Cur], Accum, Instr);
split([Char|Rest], Cur, Accum, Instr) ->
    split(Rest, [Char|Cur], Accum, Instr).

%% @private
interleave([LastToken], _Instr, [], Accum) ->
    lists:reverse([LastToken|Accum]);
interleave([Token|Tokens], [Q|Instr], [Arg|Args], Accum) ->
    interleave(Tokens, Instr, Args, [to_string(Arg, Q), Token | Accum]).

%% @private
to_string(T, _Q) when is_atom(T) ->
    erlang:atom_to_list(T);
to_string(T, _Q) when is_integer(T) ->
    erlang:integer_to_list(T);
to_string(T, _Q) when is_float(T) ->
    erlang:float_to_list(T);
to_string(T, _Q) when is_pid(T) ->
    erlang:pid_to_list(T);
to_string(T, _Q) when is_reference(T) ->
    erlang:ref_to_list(T);
to_string(T, _Q) when is_function(T) ->
    erlang:fun_to_list(T);
to_string(T, Q) when is_list(T) ->
    case is_printable_ascii(T) of
        true -> case Q of quote -> [$"] ++ T ++ [$"]; _ -> T end;
        _ ->
            "[" ++ lists:join(",", [to_string(E, quote) || E <- T]) ++ "]"
    end;
to_string(T, Q) when is_binary(T) ->
    BinList = erlang:binary_to_list(T),
    Data = case is_printable_ascii(BinList) of
        true -> case Q of quote ->[$"] ++ BinList ++ [$"]; _ -> BinList end;
        _ ->
            lists:join(",", [erlang:integer_to_list(E) || E <- BinList])
    end,
    "<<" ++ Data ++ ">>";
to_string(T, _Q) when is_tuple(T) ->
    "{" ++ lists:flatten(lists:join(",", [to_string(E, quote) || E <- erlang:tuple_to_list(T)])) ++ "}";
to_string(_T, _Q) -> "unknown".

%% @private
is_printable_ascii([]) -> true;
is_printable_ascii([E|R]) when is_integer(E) andalso 32 =< E andalso E < 127 ->
    is_printable_ascii(R);
is_printable_ascii(_) -> false.
