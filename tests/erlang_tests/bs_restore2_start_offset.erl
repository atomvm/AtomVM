%
% This file is part of AtomVM.
%
% Copyright 2022 Davide Bettio <davide@uninstall.it>
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

-module(bs_restore2_start_offset).
-export([
    parse/1, parse/3,
    start/0,
    begin_object/1,
    begin_array/1,
    end_object/1,
    end_array/1,
    end_string/2,
    end_number/2,
    got_kv_separator/1,
    end_value/1
]).

start() ->
    Json =
        <<
            "\n"
            "{\n"
            "  \"coord\": {\n"
            "    \"lon\": -135.12,\n"
            "    \"lat\": 85.22\n"
            "  },\n"
            "  \"itemsab\": [\n"
            "    {\n"
            "      \"id\": 700,\n"
            "      \"main\": \"MainA\",\n"
            "      \"description\": \"abcde fgh\",\n"
            "      \"icon\": \"53c\"\n"
            "    }\n"
            "  ],\n"
            "  \"abcd\": \"qwertyui\",\n"
            "  \"main\": {\n"
            "    \"asdf\": 252.44,\n"
            "    \"asdfghjklz\": 321.85,\n"
            "    \"zxcvbnmq\": 444.23,\n"
            "    \"qertyuio\": 285.38,\n"
            "    \"qscefvth\": 1023,\n"
            "    \"ygctfcrx\": 400\n"
            "  },\n"
            "  \"okmijnuhby\": 20000,\n"
            "  \"hhhh\": {\n"
            "    \"yyyyy\": 1.5,\n"
            "    \"sss\": 640\n"
            "  },\n"
            "  \"uiohgf\": {\n"
            "    \"fgh\": 1\n"
            "  },\n"
            "  \"ab\": 1560350645,\n"
            "  \"xcv\": {\n"
            "    \"bnm1\": 1,\n"
            "    \"hj\": 5122,\n"
            "    \"rtyuiop\": 0.0139,\n"
            "    \"poiuytr\": \"AB\",\n"
            "    \"lkjhggf\": 1560343627,\n"
            "    \"mnbvcx\": 1560396563\n"
            "  },\n"
            "  \"yjkuedfr\": -36300,\n"
            "  \"nb\": 630007467,\n"
            "  \"okuh\": \"Abcdefgh Pqrs\",\n"
            "  \"rty\": 900\n"
            "  }"
        >>,
    Map = parse(Json),
    #{
        <<"itemsab">> := [#{<<"id">> := Itemsab_Id}],
        <<"main">> := #{<<"qscefvth">> := Main_Qscefvth},
        <<"rty">> := Rty
    } = Map,
    Itemsab_Id + Main_Qscefvth - Rty.

parse(<<>>, _CB, State) ->
    State;
parse(<<White, Rest/binary>>, CB, State) when
    White == $\s orelse White == $\t orelse White == $\n orelse White == $\r
->
    parse(Rest, CB, State);
parse(<<${, Rest/binary>>, CB, State) ->
    NewState = CB:begin_object(State),
    parse(Rest, CB, NewState);
parse(<<$}, Rest/binary>>, CB, State) ->
    NewState = CB:end_object(State),
    parse(Rest, CB, NewState);
parse(<<$[, Rest/binary>>, CB, State) ->
    NewState = CB:begin_array(State),
    parse(Rest, CB, NewState);
parse(<<$], Rest/binary>>, CB, State) ->
    NewState = CB:end_array(State),
    parse(Rest, CB, NewState);
parse(<<$", Rest/binary>>, CB, State) ->
    parse_string(Rest, 1, CB, State);
parse(<<$:, Rest/binary>>, CB, State) ->
    NewState = CB:got_kv_separator(State),
    parse(Rest, CB, NewState);
parse(<<$,, Rest/binary>>, CB, State) ->
    NewState = CB:end_value(State),
    parse(Rest, CB, NewState);
parse(<<Digit, _Rest/binary>> = Bin, CB, State) when Digit >= $0 andalso Digit =< $9 ->
    parse_number(Bin, 0, positive, CB, State);
parse(<<$-, Rest/binary>>, CB, State) ->
    parse_number(Rest, 0, negative, CB, State).

parse_string(In, Len, CB, State) when is_binary(In) and is_integer(Len) ->
    case In of
        <<S:Len/binary, $", Rest/binary>> ->
            NewState = CB:end_string(S, State),
            parse(Rest, CB, NewState);
        <<_S:Len/binary, _Rest/binary>> ->
            parse_string(In, Len + 1, CB, State);
        Truncated ->
            {cont, string, Truncated, Len}
    end.

parse_number(<<>>, Acc, Sign, _CB, _State) ->
    {cont, number, Acc, Sign};
parse_number(<<Digit, Rest/binary>>, Acc, Sign, CB, State) when Digit >= $0 andalso Digit =< $9 ->
    parse_number(Rest, maybe_to_float(Acc * 10 + (Digit - $0)), Sign, CB, State);
parse_number(<<$., Rest/binary>>, Acc, Sign, CB, State) ->
    parse_decimal_number(Rest, Acc, 0, zero_point_one(), Sign, CB, State);
parse_number(<<Bin/binary>>, Acc, Sign, CB, State) ->
    Num =
        case Sign of
            positive -> Acc;
            negative -> -Acc
        end,
    NewState = CB:end_number(Num, State),
    parse(Bin, CB, NewState).

parse_decimal_number(<<>>, I, Acc, M, Sign, _CB, _State) ->
    {cont, number, I, Acc, M, Sign};
parse_decimal_number(<<Digit, Rest/binary>>, I, Acc, M, Sign, CB, State) when
    Digit >= $0 andalso Digit =< $9
->
    parse_decimal_number(Rest, I, Acc + (Digit - $0) * M, M * zero_point_one(), Sign, CB, State);
parse_decimal_number(<<Bin/binary>>, I, Acc, _M, Sign, CB, State) ->
    Num =
        case Sign of
            positive -> I + Acc;
            negative -> -(I + Acc)
        end,
    NewState = CB:end_number(Num, State),
    parse(Bin, CB, NewState).

maybe_to_float(N) when is_integer(N) andalso (N > 9007199254740991 orelse N < -9007199254740991) ->
    N * one_point_zero();
maybe_to_float(N) ->
    N.

zero_point_one() ->
    erlang:binary_to_float(<<"0.1">>).

one_point_zero() ->
    erlang:binary_to_float(<<"1.0">>).

begin_object(State) ->
    [#{} | State].

end_object(State) ->
    case State of
        [#{} | _T] -> State;
        _NotEmpty -> end_value(State)
    end.

begin_array(State) ->
    [[] | State].

end_array(State) ->
    case State of
        [[] | _T] ->
            State;
        _NotEmpty ->
            [Array | T] = end_value(State),
            [reverse(Array) | T]
    end.

got_kv_separator([Bin | T] = _State) when is_binary(Bin) ->
    [[key | Bin] | T].

end_string(String, State) ->
    [String | State].

end_number(Num, State) ->
    [Num | State].

end_value([Value, [key | Key], Obj | T]) when is_map(Obj) ->
    [Obj#{Key => Value} | T];
end_value([Value, Array | T] = _L) when is_list(Array) ->
    [[Value | Array] | T].

reverse(L) ->
    reverse(L, []).

reverse([], Accum) ->
    Accum;
reverse([H | T], Accum) ->
    reverse(T, [H | Accum]);
reverse(Any, _Accum) ->
    5 = Any.

parse(Bin) ->
    case parse(Bin, ?MODULE, []) of
        [Result] -> Result;
        Error -> {error, parsing_error, Error}
    end.
