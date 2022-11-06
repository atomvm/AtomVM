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

%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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
-spec format(Format :: string(), Args :: list()) -> string().
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
    {lists:reverse([lists:reverse(Cur) | Accum]), lists:reverse(Instr)};
split([$~, $p | Rest], Cur, Accum, Instr) ->
    split(Rest, [], [lists:reverse(Cur) | Accum], [quote | Instr]);
split([$~, $s | Rest], Cur, Accum, Instr) ->
    split(Rest, [], [lists:reverse(Cur) | Accum], [literal | Instr]);
split([$~, $n | Rest], Cur, Accum, Instr) ->
    split(Rest, [$\n | Cur], Accum, Instr);
split([$~, $~ | Rest], Cur, Accum, Instr) ->
    split(Rest, [$~ | Cur], Accum, Instr);
split([Char | Rest], Cur, Accum, Instr) ->
    split(Rest, [Char | Cur], Accum, Instr).

%% @private
interleave([LastToken], _Instr, [], Accum) ->
    lists:reverse([LastToken | Accum]);
interleave([Token | Tokens], [Q | Instr], [Arg | Args], Accum) ->
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
        true ->
            case Q of
                quote -> [$"] ++ T ++ [$"];
                _ -> T
            end;
        _ ->
            "[" ++ lists:join(",", [to_string(E, quote) || E <- T]) ++ "]"
    end;
to_string(T, Q) when is_binary(T) ->
    BinList = erlang:binary_to_list(T),
    Data =
        case is_printable_ascii(BinList) of
            true ->
                case Q of
                    quote -> [$"] ++ BinList ++ [$"];
                    _ -> BinList
                end;
            _ ->
                lists:join(",", [erlang:integer_to_list(E) || E <- BinList])
        end,
    "<<" ++ Data ++ ">>";
to_string(T, _Q) when is_tuple(T) ->
    "{" ++
        lists:flatten(lists:join(",", [to_string(E, quote) || E <- erlang:tuple_to_list(T)])) ++
        "}";
to_string(T, _Q) when is_map(T) ->
    "#{" ++
        lists:flatten(
            lists:join(",", [
                to_string(K, quote) ++ " => " ++ to_string(V, quote)
             || {K, V} <- maps:to_list(T)
            ])
        ) ++ "}";
to_string(_T, _Q) ->
    "unknown".

%% @private
is_printable_ascii([]) ->
    true;
is_printable_ascii([E | R]) when is_integer(E) andalso 32 =< E andalso E < 127 ->
    is_printable_ascii(R);
is_printable_ascii(_) ->
    false.
