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

-module(json).

%% Decoder API
-export([
    decode/1,
    decode/3,
    decode_start/3,
    decode_continue/2
]).

%% Encoder API
-export([
    encode/1,
    encode/2,
    encode_value/2,
    encode_atom/2,
    encode_integer/1,
    encode_float/1,
    encode_list/2,
    encode_map/2,
    encode_map_checked/2,
    encode_key_value_list/2,
    encode_key_value_list_checked/2,
    encode_binary/1,
    encode_binary_escape_all/1
]).

%% Type exports
-export_type([
    encoder/0,
    encode_value/0,
    from_binary_fun/0,
    array_start_fun/0,
    array_push_fun/0,
    array_finish_fun/0,
    object_start_fun/0,
    object_push_fun/0,
    object_finish_fun/0,
    decoders/0,
    decode_value/0,
    continuation_state/0
]).

%%
%% Types
%%

-type encoder() :: fun((term(), encoder()) -> iodata()).

-type encode_map(Value) :: #{binary() | atom() | integer() => Value}.

-type encode_value() ::
    integer()
    | float()
    | boolean()
    | null
    | binary()
    | atom()
    | [encode_value()]
    | encode_map(encode_value()).

-type from_binary_fun() :: fun((binary()) -> term()).

-type array_start_fun() :: fun((Acc :: term()) -> ArrayAcc :: term()).
-type array_push_fun() :: fun((Value :: term(), Acc :: term()) -> NewAcc :: term()).
-type array_finish_fun() ::
    fun((ArrayAcc :: term(), OldAcc :: term()) -> {term(), term()}).

-type object_start_fun() :: fun((Acc :: term()) -> ObjectAcc :: term()).
-type object_push_fun() ::
    fun((Key :: term(), Value :: term(), Acc :: term()) -> NewAcc :: term()).
-type object_finish_fun() ::
    fun((ObjectAcc :: term(), OldAcc :: term()) -> {term(), term()}).

-type decoders() ::
    #{
        array_start => array_start_fun(),
        array_push => array_push_fun(),
        array_finish => array_finish_fun(),
        object_start => object_start_fun(),
        object_push => object_push_fun(),
        object_finish => object_finish_fun(),
        float => from_binary_fun(),
        integer => from_binary_fun(),
        string => from_binary_fun(),
        null => term()
    }.

-type decode_value() ::
    integer()
    | float()
    | boolean()
    | null
    | binary()
    | [decode_value()]
    | #{binary() => decode_value()}.

-opaque continuation_state() :: {binary(), term(), tuple()}.

-record(callbacks, {
    array_start,
    array_push,
    array_finish,
    object_start,
    object_push,
    object_finish,
    float,
    integer,
    string,
    null
}).

%%
%% Public API - Decoder
%%

%% The decoder uses an iterative stack-based approach.
%% Nesting depth is bounded only by available heap memory.
-spec decode(binary()) -> decode_value().
decode(Bin) ->
    {Value, ok, Rest} = decode(Bin, ok, #{}),
    case skip_whitespace(Rest) of
        <<>> -> Value;
        <<Byte, _/binary>> -> error({invalid_byte, Byte})
    end.

-spec decode(binary(), term(), decoders()) -> {Result :: term(), Acc :: term(), binary()}.
decode(Bin, Acc, Decoders) ->
    case decode_start(Bin, Acc, Decoders) of
        {Value, Acc1, Rest} -> {Value, Acc1, Rest};
        {continue, State} -> decode_continue(end_of_input, State)
    end.

-spec decode_start(binary(), term(), decoders()) ->
    {Result :: term(), Acc :: term(), binary()}
    | {continue, continuation_state()}.
decode_start(Bin, Acc, Decoders) ->
    Callbacks = make_callbacks(Decoders),
    try
        Bin1 = skip_whitespace(Bin),
        scan_value(Bin1, Acc, Callbacks, [])
    catch
        error:unexpected_end ->
            {continue, {Bin, Acc, Callbacks}}
    end.

%% Note: each call reparses the accumulated buffer from the start.
%% For large payloads split across many small chunks, consider
%% accumulating the full binary before calling decode/1.
-spec decode_continue(binary() | end_of_input, State :: continuation_state()) ->
    {Result :: term(), Acc :: term(), binary()}
    | {continue, continuation_state()}.
decode_continue(end_of_input, {Buf, Acc, Callbacks}) ->
    %% Append a space as delimiter to finalize any pending number,
    %% then strip the trailing space from the result.
    try
        Bin1 = skip_whitespace(<<Buf/binary, $\s>>),
        case scan_value(Bin1, Acc, Callbacks, []) of
            {Result, Acc1, <<>>} -> {Result, Acc1, <<>>};
            {Result, Acc1, <<$\s>>} -> {Result, Acc1, <<>>};
            _ -> error(unexpected_end)
        end
    catch
        error:_ ->
            error(unexpected_end)
    end;
decode_continue(NewData, {Buf, Acc, Callbacks}) ->
    Combined = <<Buf/binary, NewData/binary>>,
    try
        Bin1 = skip_whitespace(Combined),
        scan_value(Bin1, Acc, Callbacks, [])
    catch
        error:unexpected_end ->
            {continue, {Combined, Acc, Callbacks}}
    end.

%%
%% Internal: callback setup
%%

make_callbacks(Decoders) ->
    #callbacks{
        array_start = maps:get(array_start, Decoders, fun default_array_start/1),
        array_push = maps:get(array_push, Decoders, fun default_array_push/2),
        array_finish = maps:get(array_finish, Decoders, fun default_array_finish/2),
        object_start = maps:get(object_start, Decoders, fun default_object_start/1),
        object_push = maps:get(object_push, Decoders, fun default_object_push/3),
        object_finish = maps:get(object_finish, Decoders, fun default_object_finish/2),
        float = maps:get(float, Decoders, fun erlang:binary_to_float/1),
        integer = maps:get(integer, Decoders, fun erlang:binary_to_integer/1),
        string = maps:get(string, Decoders, fun default_string/1),
        null = maps:get(null, Decoders, null)
    }.

default_array_start(_) -> [].
default_array_push(Elem, Acc) -> [Elem | Acc].
default_array_finish(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc}.
default_object_start(_) -> [].
default_object_push(Key, Value, Acc) -> [{Key, Value} | Acc].
default_object_finish(Acc, OldAcc) -> {maps:from_list(Acc), OldAcc}.
default_string(V) -> binary:copy(V).

%%
%% Internal: whitespace
%%

skip_whitespace(<<$\s, Rest/binary>>) -> skip_whitespace(Rest);
skip_whitespace(<<$\t, Rest/binary>>) -> skip_whitespace(Rest);
skip_whitespace(<<$\n, Rest/binary>>) -> skip_whitespace(Rest);
skip_whitespace(<<$\r, Rest/binary>>) -> skip_whitespace(Rest);
skip_whitespace(Bin) -> Bin.

%%
%% Internal: iterative value scanner
%%
%% Uses an explicit stack (list of frames) instead of the Erlang call stack.
%% Callbacks are extracted from the tuple on use, keeping frames small.
%% Stack frame types:
%%   {array, OldAcc}
%%   {object, Key, OldAcc}

scan_value(<<"true", Rest/binary>>, Acc, Callbacks, Stack) ->
    reduce_value(true, Rest, Acc, Callbacks, Stack);
scan_value(<<"false", Rest/binary>>, Acc, Callbacks, Stack) ->
    reduce_value(false, Rest, Acc, Callbacks, Stack);
scan_value(<<"null", Rest/binary>>, Acc, Callbacks, Stack) ->
    reduce_value(Callbacks#callbacks.null, Rest, Acc, Callbacks, Stack);
scan_value(<<$", Rest/binary>>, Acc, Callbacks, Stack) ->
    {Value, Acc1, Rest1} = string_fast(Rest, Rest, Acc, Callbacks),
    reduce_value(Value, Rest1, Acc1, Callbacks, Stack);
scan_value(<<$[, Rest/binary>>, Acc, Callbacks, Stack) ->
    ArrayAcc = (Callbacks#callbacks.array_start)(Acc),
    Rest1 = skip_whitespace(Rest),
    case Rest1 of
        <<$], Rest2/binary>> ->
            {Result, NewAcc} = (Callbacks#callbacks.array_finish)(ArrayAcc, Acc),
            reduce_value(Result, Rest2, NewAcc, Callbacks, Stack);
        _ ->
            scan_value(Rest1, ArrayAcc, Callbacks, [{array, Acc} | Stack])
    end;
scan_value(<<${, Rest/binary>>, Acc, Callbacks, Stack) ->
    ObjAcc = (Callbacks#callbacks.object_start)(Acc),
    Rest1 = skip_whitespace(Rest),
    case Rest1 of
        <<$}, Rest2/binary>> ->
            {Result, NewAcc} = (Callbacks#callbacks.object_finish)(ObjAcc, Acc),
            reduce_value(Result, Rest2, NewAcc, Callbacks, Stack);
        <<$", Rest2/binary>> ->
            {Key, ObjAcc1, Rest3} = string_fast(Rest2, Rest2, ObjAcc, Callbacks),
            Rest4 = skip_whitespace(Rest3),
            case Rest4 of
                <<$:, Rest5/binary>> ->
                    Rest6 = skip_whitespace(Rest5),
                    scan_value(Rest6, ObjAcc1, Callbacks, [{object, Key, Acc} | Stack]);
                <<Byte, _/binary>> ->
                    error({invalid_byte, Byte});
                <<>> ->
                    error(unexpected_end)
            end;
        <<Byte, _/binary>> ->
            error({invalid_byte, Byte});
        <<>> ->
            error(unexpected_end)
    end;
scan_value(<<$-, _/binary>> = Bin, Acc, Callbacks, Stack) ->
    {Value, Acc1, Rest} = parse_number(Bin, Acc, Callbacks),
    reduce_value(Value, Rest, Acc1, Callbacks, Stack);
scan_value(<<D, _/binary>> = Bin, Acc, Callbacks, Stack) when D >= $0, D =< $9 ->
    {Value, Acc1, Rest} = parse_number(Bin, Acc, Callbacks),
    reduce_value(Value, Rest, Acc1, Callbacks, Stack);
%% Partial literals (for streaming: input may be truncated mid-keyword)
scan_value(<<$t, _/binary>> = Bin, _, _, _) ->
    maybe_partial_literal(Bin, <<"true">>, $t);
scan_value(<<$f, _/binary>> = Bin, _, _, _) ->
    maybe_partial_literal(Bin, <<"false">>, $f);
scan_value(<<$n, _/binary>> = Bin, _, _, _) ->
    maybe_partial_literal(Bin, <<"null">>, $n);
scan_value(<<Byte, _/binary>>, _, _, _) ->
    error({invalid_byte, Byte});
scan_value(<<>>, _, _, _) ->
    error(unexpected_end).

%% After parsing a value, attach it to the parent container or return at top level.
%% Match OTP: consume trailing whitespace only when rest is whitespace-only.
reduce_value(Value, Rest, Acc, _Callbacks, []) ->
    case skip_whitespace(Rest) of
        <<>> -> {Value, Acc, <<>>};
        _ -> {Value, Acc, Rest}
    end;
reduce_value(Value, Rest0, Acc, Callbacks, [{array, OldAcc} | StackRest] = Stack) ->
    ArrayAcc = (Callbacks#callbacks.array_push)(Value, Acc),
    Rest1 = skip_whitespace(Rest0),
    case Rest1 of
        <<$], Rest2/binary>> ->
            {Result, NewAcc} = (Callbacks#callbacks.array_finish)(ArrayAcc, OldAcc),
            reduce_value(Result, Rest2, NewAcc, Callbacks, StackRest);
        <<$,, Rest2/binary>> ->
            Rest3 = skip_whitespace(Rest2),
            scan_value(Rest3, ArrayAcc, Callbacks, Stack);
        <<Byte, _/binary>> ->
            error({invalid_byte, Byte});
        <<>> ->
            error(unexpected_end)
    end;
reduce_value(Value, Rest0, Acc, Callbacks, [{object, Key, OldAcc} | StackRest]) ->
    ObjAcc = (Callbacks#callbacks.object_push)(Key, Value, Acc),
    Rest1 = skip_whitespace(Rest0),
    case Rest1 of
        <<$}, Rest2/binary>> ->
            {Result, NewAcc} = (Callbacks#callbacks.object_finish)(ObjAcc, OldAcc),
            reduce_value(Result, Rest2, NewAcc, Callbacks, StackRest);
        <<$,, Rest2/binary>> ->
            Rest3 = skip_whitespace(Rest2),
            case Rest3 of
                <<$", Rest4/binary>> ->
                    {NewKey, ObjAcc1, Rest5} = string_fast(Rest4, Rest4, ObjAcc, Callbacks),
                    Rest6 = skip_whitespace(Rest5),
                    case Rest6 of
                        <<$:, Rest7/binary>> ->
                            Rest8 = skip_whitespace(Rest7),
                            scan_value(
                                Rest8,
                                ObjAcc1,
                                Callbacks,
                                [{object, NewKey, OldAcc} | StackRest]
                            );
                        <<Byte, _/binary>> ->
                            error({invalid_byte, Byte});
                        <<>> ->
                            error(unexpected_end)
                    end;
                <<Byte, _/binary>> ->
                    error({invalid_byte, Byte});
                <<>> ->
                    error(unexpected_end)
            end;
        <<Byte, _/binary>> ->
            error({invalid_byte, Byte});
        <<>> ->
            error(unexpected_end)
    end.

maybe_partial_literal(Bin, Expected, Byte) ->
    Len = byte_size(Bin),
    case Expected of
        <<Prefix:Len/binary, _/binary>> when Prefix =:= Bin -> error(unexpected_end);
        _ -> error({invalid_byte, Byte})
    end.

%%
%% Internal: string parser
%%

%% Fast path: no escapes. Walk with /utf8 matching for automatic UTF-8 validation.
%% Orig = binary starting after opening quote (for binary_part extraction).
%% Position computed from byte_size at $" or $\ -- no Pos counter needed.
string_fast(<<$", Rest/binary>>, Orig, Acc, Callbacks) ->
    Pos = byte_size(Orig) - byte_size(Rest) - 1,
    Str = binary_part(Orig, 0, Pos),
    apply_string_decoder(Str, Acc, Rest, Callbacks);
string_fast(<<$\\, Rest/binary>>, Orig, Acc, Callbacks) ->
    Pos = byte_size(Orig) - byte_size(Rest) - 1,
    Prefix = binary_part(Orig, 0, Pos),
    string_escape(Rest, [Prefix], Acc, Callbacks);
string_fast(<<Ch/utf8, _/binary>>, _, _, _) when Ch < 16#20 ->
    error({invalid_byte, Ch});
string_fast(<<_/utf8, Rest/binary>>, Orig, Acc, Callbacks) ->
    string_fast(Rest, Orig, Acc, Callbacks);
%% Maybe truncated UTF-8 sequence at end of buffer. Could continue
string_fast(<<B, Rest/binary>>, _, _, _) when
    byte_size(Rest) < 3,
    (B >= 16#C0 andalso B < 16#E0 andalso byte_size(Rest) < 1) orelse
        (B >= 16#E0 andalso B < 16#F0 andalso byte_size(Rest) < 2) orelse
        (B >= 16#F0 andalso B < 16#F8 andalso byte_size(Rest) < 3)
->
    error(unexpected_end);
%% Invalid UTF-8 byte (not matched by /utf8 above)
string_fast(<<B, _/binary>>, _, _, _) ->
    error({invalid_byte, B});
string_fast(<<>>, _, _, _) ->
    error(unexpected_end).

apply_string_decoder(Str, Acc, Rest, Callbacks) ->
    StringFun = Callbacks#callbacks.string,
    {StringFun(Str), Acc, Rest}.

%% Slow path: has escapes. Accumulate decoded chunks in reversed iolist.
%% Enter a new "run" scanning for the next escape or closing quote.
string_slow(Bin, Parts, Acc, Callbacks) ->
    string_slow_run(Bin, Bin, Parts, Acc, Callbacks).

string_slow_run(<<$", Rest/binary>>, RunOrig, Parts, Acc, Callbacks) ->
    RunLen = byte_size(RunOrig) - byte_size(Rest) - 1,
    Run = binary_part(RunOrig, 0, RunLen),
    Str = iolist_to_binary(lists:reverse([Run | Parts])),
    apply_string_decoder(Str, Acc, Rest, Callbacks);
string_slow_run(<<$\\, Rest/binary>>, RunOrig, Parts, Acc, Callbacks) ->
    RunLen = byte_size(RunOrig) - byte_size(Rest) - 1,
    Run = binary_part(RunOrig, 0, RunLen),
    string_escape(Rest, [Run | Parts], Acc, Callbacks);
string_slow_run(<<Ch/utf8, _/binary>>, _, _, _, _) when Ch < 16#20 ->
    error({invalid_byte, Ch});
string_slow_run(<<_/utf8, Rest/binary>>, RunOrig, Parts, Acc, Callbacks) ->
    string_slow_run(Rest, RunOrig, Parts, Acc, Callbacks);
%% Maybe truncated UTF-8 sequence at end of buffer. Could continue
string_slow_run(<<B, Rest/binary>>, _, _, _, _) when
    byte_size(Rest) < 3,
    (B >= 16#C0 andalso B < 16#E0 andalso byte_size(Rest) < 1) orelse
        (B >= 16#E0 andalso B < 16#F0 andalso byte_size(Rest) < 2) orelse
        (B >= 16#F0 andalso B < 16#F8 andalso byte_size(Rest) < 3)
->
    error(unexpected_end);
string_slow_run(<<B, _/binary>>, _, _, _, _) ->
    error({invalid_byte, B});
string_slow_run(<<>>, _, _, _, _) ->
    error(unexpected_end).

%% Escape character dispatch
string_escape(<<$", Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$">> | Parts], Acc, Callbacks);
string_escape(<<$\\, Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$\\>> | Parts], Acc, Callbacks);
string_escape(<<$/, Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$/>> | Parts], Acc, Callbacks);
string_escape(<<$b, Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$\b>> | Parts], Acc, Callbacks);
string_escape(<<$f, Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$\f>> | Parts], Acc, Callbacks);
string_escape(<<$n, Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$\n>> | Parts], Acc, Callbacks);
string_escape(<<$r, Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$\r>> | Parts], Acc, Callbacks);
string_escape(<<$t, Rest/binary>>, Parts, Acc, Callbacks) ->
    string_slow(Rest, [<<$\t>> | Parts], Acc, Callbacks);
string_escape(<<$u, Rest/binary>>, Parts, Acc, Callbacks) ->
    parse_unicode_escape(Rest, Parts, Acc, Callbacks);
string_escape(<<Byte, _/binary>>, _, _, _) ->
    error({invalid_byte, Byte});
string_escape(<<>>, _, _, _) ->
    error(unexpected_end).

%% Unicode escape: \uXXXX with surrogate pair support
parse_unicode_escape(<<H1, H2, H3, H4, Rest/binary>>, Parts, Acc, Callbacks) ->
    case safe_hex4(H1, H2, H3, H4) of
        error ->
            error({unexpected_sequence, <<$\\, $u, H1, H2, H3, H4>>});
        CP when CP >= 16#D800, CP =< 16#DBFF ->
            %% High surrogate - must be followed by low surrogate \uDC00-\uDFFF
            parse_surrogate_low(Rest, CP, H1, H2, H3, H4, Parts, Acc, Callbacks);
        CP when CP >= 16#DC00, CP =< 16#DFFF ->
            %% Lone low surrogate
            error({unexpected_sequence, <<$\\, $u, H1, H2, H3, H4>>});
        CP ->
            Utf8 = <<CP/utf8>>,
            string_slow(Rest, [Utf8 | Parts], Acc, Callbacks)
    end;
parse_unicode_escape(_, _, _, _) ->
    error(unexpected_end).

parse_surrogate_low(
    <<$\\, $u, L1, L2, L3, L4, Rest/binary>>, High, H1, H2, H3, H4, Parts, Acc, Callbacks
) ->
    case safe_hex4(L1, L2, L3, L4) of
        error ->
            error(
                {unexpected_sequence, <<$\\, $u, H1, H2, H3, H4, $\\, $u, L1, L2, L3, L4>>}
            );
        Low when Low >= 16#DC00, Low =< 16#DFFF ->
            Combined = 16#10000 + ((High - 16#D800) bsl 10) + (Low - 16#DC00),
            Utf8 = <<Combined/utf8>>,
            string_slow(Rest, [Utf8 | Parts], Acc, Callbacks);
        _Low ->
            error(
                {unexpected_sequence, <<$\\, $u, H1, H2, H3, H4, $\\, $u, L1, L2, L3, L4>>}
            )
    end;
parse_surrogate_low(<<>>, _, _H1, _H2, _H3, _H4, _, _, _) ->
    error(unexpected_end);
parse_surrogate_low(<<$\\>>, _, _H1, _H2, _H3, _H4, _, _, _) ->
    error(unexpected_end);
parse_surrogate_low(<<$\\, $u, _/binary>>, _, _H1, _H2, _H3, _H4, _, _, _) ->
    %% \u present but fewer than 4 hex digits
    error(unexpected_end);
parse_surrogate_low(_, _, H1, H2, H3, H4, _, _, _) ->
    error({unexpected_sequence, <<$\\, $u, H1, H2, H3, H4>>}).

safe_hex4(H1, H2, H3, H4) ->
    case {safe_hex(H1), safe_hex(H2), safe_hex(H3), safe_hex(H4)} of
        {V1, V2, V3, V4} when
            is_integer(V1),
            is_integer(V2),
            is_integer(V3),
            is_integer(V4)
        ->
            (V1 bsl 12) bor (V2 bsl 8) bor (V3 bsl 4) bor V4;
        _ ->
            error
    end.

safe_hex(D) when D >= $0, D =< $9 -> D - $0;
safe_hex(D) when D >= $a, D =< $f -> D - $a + 10;
safe_hex(D) when D >= $A, D =< $F -> D - $A + 10;
safe_hex(_) -> error.

%%
%% Internal: number parser
%%
%% State machine following RFC 8259 Section 6:
%%   number = [ minus ] int [ frac ] [ exp ]
%%   int = zero / ( digit1-9 *DIGIT )
%%   frac = decimal-point 1*DIGIT
%%   exp = e [ minus / plus ] 1*DIGIT

parse_number(<<$-, Rest/binary>> = Bin, Acc, Callbacks) ->
    number_minus(Rest, Bin, 1, Acc, Callbacks);
parse_number(<<$0, Rest/binary>> = Bin, Acc, Callbacks) ->
    number_zero(Rest, Bin, 1, Acc, Callbacks);
parse_number(<<D, Rest/binary>> = Bin, Acc, Callbacks) when D >= $1, D =< $9 ->
    number_digits(Rest, Bin, 1, Acc, Callbacks).

%% After minus: need at least one digit
number_minus(<<$0, Rest/binary>>, Bin, Pos, Acc, Callbacks) ->
    number_zero(Rest, Bin, Pos + 1, Acc, Callbacks);
number_minus(<<D, Rest/binary>>, Bin, Pos, Acc, Callbacks) when D >= $1, D =< $9 ->
    number_digits(Rest, Bin, Pos + 1, Acc, Callbacks);
number_minus(<<Byte, _/binary>>, _, _, _, _) ->
    error({invalid_byte, Byte});
number_minus(<<>>, _, _, _, _) ->
    error(unexpected_end).

%% After leading zero: only dot, exponent, or end allowed (no leading zeros like 01)
number_zero(<<$., Rest/binary>>, Bin, Pos, Acc, Callbacks) ->
    number_frac_first(Rest, Bin, Pos + 1, Acc, Callbacks);
number_zero(<<E, Rest/binary>>, Bin, Pos, Acc, Callbacks) when E =:= $e; E =:= $E ->
    number_exp_sign(Rest, Bin, Pos + 1, float_exp, Acc, Callbacks);
number_zero(<<>>, _, _, _, _) ->
    error(unexpected_end);
number_zero(Rest, Bin, Pos, Acc, Callbacks) ->
    number_complete(Rest, Bin, Pos, integer, Acc, Callbacks).

%% Integer digits (first digit was 1-9)
number_digits(<<D, Rest/binary>>, Bin, Pos, Acc, Callbacks) when D >= $0, D =< $9 ->
    number_digits(Rest, Bin, Pos + 1, Acc, Callbacks);
number_digits(<<$., Rest/binary>>, Bin, Pos, Acc, Callbacks) ->
    number_frac_first(Rest, Bin, Pos + 1, Acc, Callbacks);
number_digits(<<E, Rest/binary>>, Bin, Pos, Acc, Callbacks) when E =:= $e; E =:= $E ->
    number_exp_sign(Rest, Bin, Pos + 1, float_exp, Acc, Callbacks);
number_digits(<<>>, _, _, _, _) ->
    error(unexpected_end);
number_digits(Rest, Bin, Pos, Acc, Callbacks) ->
    number_complete(Rest, Bin, Pos, integer, Acc, Callbacks).

%% After dot: need at least one digit
number_frac_first(<<D, Rest/binary>>, Bin, Pos, Acc, Callbacks) when D >= $0, D =< $9 ->
    number_frac(Rest, Bin, Pos + 1, Acc, Callbacks);
number_frac_first(<<Byte, _/binary>>, _, _, _, _) ->
    error({invalid_byte, Byte});
number_frac_first(<<>>, _, _, _, _) ->
    error(unexpected_end).

%% Fraction digits
number_frac(<<D, Rest/binary>>, Bin, Pos, Acc, Callbacks) when D >= $0, D =< $9 ->
    number_frac(Rest, Bin, Pos + 1, Acc, Callbacks);
number_frac(<<E, Rest/binary>>, Bin, Pos, Acc, Callbacks) when E =:= $e; E =:= $E ->
    number_exp_sign(Rest, Bin, Pos + 1, float, Acc, Callbacks);
number_frac(<<>>, _, _, _, _) ->
    error(unexpected_end);
number_frac(Rest, Bin, Pos, Acc, Callbacks) ->
    number_complete(Rest, Bin, Pos, float, Acc, Callbacks).

%% Exponent: optional sign, then digits
%% Type = float (had dot) | float_exp (no dot, needs normalization)
number_exp_sign(<<S, Rest/binary>>, Bin, Pos, Type, Acc, Callbacks) when S =:= $+; S =:= $- ->
    number_exp_first(Rest, Bin, Pos + 1, Type, Acc, Callbacks);
number_exp_sign(Rest, Bin, Pos, Type, Acc, Callbacks) ->
    number_exp_first(Rest, Bin, Pos, Type, Acc, Callbacks).

%% After e/E [+/-]: need at least one digit
number_exp_first(<<D, Rest/binary>>, Bin, Pos, Type, Acc, Callbacks) when D >= $0, D =< $9 ->
    number_exp_digits(Rest, Bin, Pos + 1, Type, Acc, Callbacks);
number_exp_first(<<Byte, _/binary>>, _, _, _, _, _) ->
    error({invalid_byte, Byte});
number_exp_first(<<>>, _, _, _, _, _) ->
    error(unexpected_end).

%% Exponent digits
number_exp_digits(<<D, Rest/binary>>, Bin, Pos, Type, Acc, Callbacks) when D >= $0, D =< $9 ->
    number_exp_digits(Rest, Bin, Pos + 1, Type, Acc, Callbacks);
number_exp_digits(<<>>, _, _, _, _, _) ->
    error(unexpected_end);
number_exp_digits(Rest, Bin, Pos, Type, Acc, Callbacks) ->
    number_complete(Rest, Bin, Pos, Type, Acc, Callbacks).

%% Extract number binary and convert
number_complete(Rest, Orig, Pos, integer, Acc, Callbacks) ->
    NumBin = binary_part(Orig, 0, Pos),
    IntFun = Callbacks#callbacks.integer,
    {IntFun(NumBin), Acc, Rest};
number_complete(Rest, Orig, Pos, float, Acc, Callbacks) ->
    NumBin = binary_part(Orig, 0, Pos),
    FloatFun = Callbacks#callbacks.float,
    {FloatFun(NumBin), Acc, Rest};
number_complete(Rest, Orig, Pos, float_exp, Acc, Callbacks) ->
    NumBin = binary_part(Orig, 0, Pos),
    NormBin = normalize_float(NumBin),
    FloatFun = Callbacks#callbacks.float,
    {FloatFun(NormBin), Acc, Rest}.

%% Insert ".0" before e/E for numbers like "1e5" -> "1.0e5"
%% (Erlang's binary_to_float requires a decimal point)
normalize_float(Bin) ->
    normalize_float(Bin, Bin, 0).

normalize_float(<<E, _/binary>>, Orig, Pos) when E =:= $e; E =:= $E ->
    IntPart = binary_part(Orig, 0, Pos),
    ExpPart = binary_part(Orig, Pos, byte_size(Orig) - Pos),
    iolist_to_binary([IntPart, ".0", ExpPart]);
normalize_float(<<_, Rest/binary>>, Orig, Pos) ->
    normalize_float(Rest, Orig, Pos + 1).

%%
%% Encoder
%%

-spec encode(encode_value()) -> iodata().
encode(Term) ->
    encode(Term, fun encode_value/2).

-spec encode(term(), encoder()) -> iodata().
encode(Term, Encoder) ->
    Encoder(Term, Encoder).

-spec encode_value(term(), encoder()) -> iodata().
encode_value(Atom, Encoder) when is_atom(Atom) -> encode_atom(Atom, Encoder);
encode_value(Bin, _Encoder) when is_binary(Bin) -> encode_binary(Bin);
encode_value(Int, _Encoder) when is_integer(Int) -> encode_integer(Int);
encode_value(Float, _Encoder) when is_float(Float) -> encode_float(Float);
encode_value(List, Encoder) when is_list(List) -> encode_list(List, Encoder);
encode_value(Map, Encoder) when is_map(Map) -> encode_map(Map, Encoder);
encode_value(Other, _Encoder) -> error({unsupported_type, Other}).

-spec encode_atom(atom(), encoder()) -> iodata().
encode_atom(null, _) -> <<"null">>;
encode_atom(true, _) -> <<"true">>;
encode_atom(false, _) -> <<"false">>;
encode_atom(Atom, Encoder) -> Encoder(atom_to_binary(Atom, utf8), Encoder).

-spec encode_integer(integer()) -> iodata().
encode_integer(Int) ->
    integer_to_binary(Int).

-spec encode_float(float()) -> iodata().
encode_float(Float) ->
    float_to_binary(Float, [short]).

-spec encode_list(list(), encoder()) -> iodata().
encode_list([], _Encoder) ->
    <<"[]">>;
encode_list([H | T], Encoder) ->
    [$[, Encoder(H, Encoder) | encode_list_loop(T, Encoder)].

encode_list_loop([], _) -> [$]];
encode_list_loop([H | T], Encoder) -> [$,, Encoder(H, Encoder) | encode_list_loop(T, Encoder)].

%%
%% Encoder: string encoding
%%

-spec encode_binary(binary()) -> iodata().
encode_binary(Bin) ->
    [$", escape(Bin, Bin, [], false), $"].

-spec encode_binary_escape_all(binary()) -> iodata().
encode_binary_escape_all(Bin) ->
    [$", escape(Bin, Bin, [], true), $"].

%% Shared escape scanner.
%% Orig tracks start of current unescaped run (for binary_part extraction).
%% Acc = [] means fast path (no escapes found yet); non-[] means building iolist.
%% EscapeAll: true escapes non-ASCII (>= 0x80) to \uXXXX, false passes through.
escape(<<>>, Orig, [], _) ->
    Orig;
escape(<<>>, RunOrig, Acc, _) ->
    lists:reverse([RunOrig | Acc]);
escape(<<$", Rest/binary>>, Orig, Acc, EA) ->
    escape_found(Orig, Rest, <<"\\\"">>, Acc, EA);
escape(<<$\\, Rest/binary>>, Orig, Acc, EA) ->
    escape_found(Orig, Rest, <<"\\\\">>, Acc, EA);
escape(<<Ch/utf8, Rest/binary>>, Orig, Acc, EA) when Ch < 16#20 ->
    escape_found(Orig, Rest, escape_ctrl(Ch), Acc, EA);
escape(<<Ch/utf8, Rest/binary>>, Orig, Acc, true) when Ch >= 16#80 ->
    Pos = byte_size(Orig) - byte_size(Rest) - utf8_byte_len(Ch),
    Run = binary_part(Orig, 0, Pos),
    escape(Rest, Rest, [escape_unicode(Ch), Run | Acc], true);
escape(<<_/utf8, Rest/binary>>, Orig, Acc, EA) ->
    escape(Rest, Orig, Acc, EA);
%% Maybe truncated UTF-8 sequence at end of binary
escape(<<B, Rest/binary>>, _, _, _) when
    byte_size(Rest) < 3,
    (B >= 16#C0 andalso B < 16#E0 andalso byte_size(Rest) < 1) orelse
        (B >= 16#E0 andalso B < 16#F0 andalso byte_size(Rest) < 2) orelse
        (B >= 16#F0 andalso B < 16#F8 andalso byte_size(Rest) < 3)
->
    error(unexpected_end);
escape(<<B, _/binary>>, _, _, _) ->
    error({invalid_byte, B}).

escape_found(Orig, Rest, Replacement, Acc, EA) ->
    Pos = byte_size(Orig) - byte_size(Rest) - 1,
    Run = binary_part(Orig, 0, Pos),
    escape(Rest, Rest, [Replacement, Run | Acc], EA).

escape_ctrl($\b) -> <<"\\b">>;
escape_ctrl($\t) -> <<"\\t">>;
escape_ctrl($\n) -> <<"\\n">>;
escape_ctrl($\f) -> <<"\\f">>;
escape_ctrl($\r) -> <<"\\r">>;
escape_ctrl(Ch) -> <<"\\u00", (hex_char(Ch bsr 4)), (hex_char(Ch band 16#F))>>.

escape_unicode(Ch) when Ch < 16#10000 ->
    <<
        $\\,
        $u,
        (hex_char(Ch bsr 12)),
        (hex_char((Ch bsr 8) band 16#F)),
        (hex_char((Ch bsr 4) band 16#F)),
        (hex_char(Ch band 16#F))
    >>;
escape_unicode(Ch) ->
    Ch1 = Ch - 16#10000,
    High = 16#D800 + (Ch1 bsr 10),
    Low = 16#DC00 + (Ch1 band 16#3FF),
    <<
        $\\,
        $u,
        (hex_char(High bsr 12)),
        (hex_char((High bsr 8) band 16#F)),
        (hex_char((High bsr 4) band 16#F)),
        (hex_char(High band 16#F)),
        $\\,
        $u,
        (hex_char(Low bsr 12)),
        (hex_char((Low bsr 8) band 16#F)),
        (hex_char((Low bsr 4) band 16#F)),
        (hex_char(Low band 16#F))
    >>.

utf8_byte_len(Ch) when Ch < 16#800 -> 2;
utf8_byte_len(Ch) when Ch < 16#10000 -> 3;
utf8_byte_len(_) -> 4.

hex_char(N) when N < 10 -> $0 + N;
hex_char(N) -> $A + N - 10.

%%
%% Encoder: object encoding (maps and key-value lists)
%%

-spec encode_map(encode_map(term()), encoder()) -> iodata().
encode_map(Map, Encoder) ->
    encode_object(fun maps:next/1, maps:iterator(Map), Encoder).

-spec encode_key_value_list([{term(), term()}], encoder()) -> iodata().
encode_key_value_list(List, Encoder) ->
    encode_object(fun kv_next/1, List, Encoder).

-spec encode_map_checked(map(), encoder()) -> iodata().
encode_map_checked(Map, Encoder) ->
    encode_object_checked(fun maps:next/1, maps:iterator(Map), Encoder).

-spec encode_key_value_list_checked([{term(), term()}], encoder()) -> iodata().
encode_key_value_list_checked(List, Encoder) ->
    encode_object_checked(fun kv_next/1, List, Encoder).

encode_object(IterFun, State, Encoder) ->
    case IterFun(State) of
        none ->
            <<"{}">>;
        {Key, Value, Next} ->
            [
                ${,
                encode_key(Key, Encoder),
                $:,
                Encoder(Value, Encoder)
                | encode_object_rest(IterFun, Next, Encoder)
            ]
    end.

encode_object_rest(IterFun, State, Encoder) ->
    case IterFun(State) of
        none ->
            [$}];
        {Key, Value, Next} ->
            [
                $,,
                encode_key(Key, Encoder),
                $:,
                Encoder(Value, Encoder)
                | encode_object_rest(IterFun, Next, Encoder)
            ]
    end.

encode_object_checked(IterFun, State, Encoder) ->
    case IterFun(State) of
        none ->
            <<"{}">>;
        {Key, Value, Next} ->
            EncodedKey = iolist_to_binary(encode_key(Key, Encoder)),
            [
                ${,
                EncodedKey,
                $:,
                Encoder(Value, Encoder)
                | encode_object_rest_checked(IterFun, Next, Encoder, #{EncodedKey => Key})
            ]
    end.

encode_object_rest_checked(IterFun, State, Encoder, Seen) ->
    case IterFun(State) of
        none ->
            [$}];
        {Key, Value, Next} ->
            EncodedKey = iolist_to_binary(encode_key(Key, Encoder)),
            case Seen of
                #{EncodedKey := _} -> error({duplicate_key, Key});
                _ -> ok
            end,
            [
                $,,
                EncodedKey,
                $:,
                Encoder(Value, Encoder)
                | encode_object_rest_checked(IterFun, Next, Encoder, Seen#{EncodedKey => Key})
            ]
    end.

encode_key(Key, Encoder) when is_binary(Key) -> Encoder(Key, Encoder);
encode_key(Key, Encoder) when is_atom(Key) -> Encoder(atom_to_binary(Key, utf8), Encoder);
encode_key(Key, _Encoder) when is_integer(Key) -> encode_binary(integer_to_binary(Key));
encode_key(Key, _Encoder) when is_float(Key) -> encode_binary(float_to_binary(Key, [short])).

kv_next([]) -> none;
kv_next([{K, V} | T]) -> {K, V, T}.
