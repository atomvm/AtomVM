%
% This file is part of AtomVM.
%
% Copyright 2019-2021 Fred Dushin <fred@dushin.net>
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

-module(test_bs).

-export([start/0, id/1]).

start() ->
    test_pack_small_ints({2, 61, 20}, <<23, 180>>),
    test_pack_integer_big_endian(1024, 32, <<0, 0, 4, 0>>),
    IntegersAndBinaries = test_pack_integers_and_binaries(
        16#F,
        16#2,
        <<"fubar">>,
        <<"Haddock's Eyes">>,
        <<0, 0, 0, 15, 0, 2, 102, 117, 98, 97, 114, 72, 97, 100>>
    ),
    test_unpack_integers_and_binaries(IntegersAndBinaries, 16#F, 16#2, <<"fubar">>, <<"Had">>),

    ok = test_create_with_invalid_int_value(),
    ok = test_create_with_invalid_int_size(),
    ok = test_create_with_int_unit(),
    ok = test_create_with_unsupported_unaligned_int_size(),
    ok = test_create_with_int_little_endian(),
    ok = test_create_with_int_signed(),
    ok = test_create_with_invalid_binary_value(),
    ok = test_create_with_invalid_binary_size(),
    ok = test_create_with_binary_size_out_of_range(),
    ok = test_create_with_unsupported_binary_unit(),

    15 = get_integer_big_unsigned(<<16#F>>, 8),
    128 = get_integer_big_unsigned(<<16#80>>, 8),
    4404 = get_integer_big_unsigned(<<0, 0, 17, 52>>, 32),

    ok = test_get_with_unsupported_int_unit(),
    ok = test_get_with_int_little_endian(),
    ok = test_get_with_int_signed(),
    ok = test_get_with_unaligned_binary(),

    <<"">> = test_match_first_integer(<<16#FF>>),
    <<1, 2, 3>> = test_match_first_integer(<<16#FF, 1, 2, 3>>),
    <<1, 2, 3>> = test_match_first_integer(<<16#AB, 16#CD, 1, 2, 3>>),
    nope = test_match_first_integer(<<16#00, 1, 2, 3>>),

    <<1, 2, 3, 1, 2, 3, 4, 5, 6>> = test_bs_append(<<1, 2, 3>>, <<1, 2, 3, 4, 5, 6>>),

    <<1, 2, 3>> = test_bs_private_append(<<1, 2, 3>>),

    nope = test_match_clause(<<"">>),
    nope = test_match_clause(<<16#FF>>),
    nope = test_match_clause(<<$n:8>>),
    nope = test_match_clause(<<$n:8, 1, 2, 3>>),
    {$n, <<1, 2, 3, 4>>, <<"">>} = test_match_clause(<<$n:8, 1, 2, 3, 4>>),
    {$n, <<1, 2, 3, 4>>, <<5, 6>>} = test_match_clause(<<$n:8, 1, 2, 3, 4, 5, 6>>),

    [<<"">>] = test_match_recursive(<<"">>, []),
    [<<"">>, 119] = test_match_recursive(<<119:32>>, []),
    [<<"">>, 119, 122] = test_match_recursive(<<122:8, 119:32>>, []),
    [<<"">>, 122, 122, 119, 119, 119, 122] = test_match_recursive(
        <<122:8, 119:32, 119:32, 119:32, 122:8, 122:8>>,
        []
    ),
    nope = test_match_recursive(<<"foo">>, []),

    BigBin = make_binary(1025),
    FirstPart = binary:part(BigBin, 0, 1024),
    LastPart = binary:part(BigBin, 1024, 1),
    {FirstPart, LastPart} = test_match_force_gc(BigBin),

    test_put_match_string(<<"foo">>, <<"bar">>),
    test_skip_bits(),

    test_match_case_type(),

    ok = test_iterate_binary(),

    ok = test_large(),

    ok = test_copy_bits_string(),

    0.

test_pack_small_ints({A, B, C}, Expect) ->
    Expect = <<A:5, B:6, C:5>>,
    Expect.

test_pack_integer_big_endian(Int, Size, Expect) ->
    Expect = <<Int:Size>>,
    Expect.

test_pack_integers_and_binaries(Int1, Int2, Bin1, Bin2, Expect) ->
    Bin = <<Int1:32/big, Int2:16, Bin1/binary, Bin2:3/binary>>,
    %% erlang:display(Bin),
    Expect = Bin,
    Expect.

test_unpack_integers_and_binaries(Bin, Int1, Int2, Bin1, Bin2) ->
    <<A:32, B:16, C:5/binary, D/binary>> = Bin,
    A = Int1,
    B = Int2,
    C = Bin1,
    D = Bin2,
    Bin.

test_create_with_invalid_int_value() ->
    expect_error(fun() -> create_int_binary(foo, id(32)) end, badarg).

test_create_with_invalid_int_size() ->
    expect_error(fun() -> create_int_binary(16#F, id(bar)) end, badarg).

test_create_with_int_unit() ->
    <<1, 66, 67>> = create_int_binary_unit_3(16#14243, id(8)),
    <<0, 0, 0, 1, 66, 67>> = create_int_binary_unit_3(16#14243, id(16)),
    <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15>> = create_int_binary_unit_3(16#F, id(32)),
    <<0, 0, 0, 0, 16#12, 16#34, 16#56, 16#78, 16#90, 16#AB, 16#CD, 16#EF>> = create_int_binary_unit_3(
        16#1234567890ABCDEF, id(32)
    ),
    ok.

test_create_with_unsupported_unaligned_int_size() ->
    atom_unsupported(fun() -> create_int_binary(16#FFFF, id(28)) end).

test_create_with_int_little_endian() ->
    ok = expect_equals(<<255, 255, 0, 0>>, create_int_binary_little_endian(16#FFFF, 32)),
    ok = expect_equals(<<0, 4, 0, 0>>, create_int_binary_little_endian(1024, 32)),
    ok = expect_equals(<<0>>, create_int_binary_little_endian(1024, 8)),
    ok.

test_create_with_int_signed() ->
    ok = expect_equals(<<0, 0, 255, 255>>, create_int_binary_signed(16#FFFF, 32)),
    ok.

test_create_with_invalid_binary_value() ->
    expect_error(fun() -> create_binary_binary(foo, id(32)) end, badarg).

test_create_with_invalid_binary_size() ->
    expect_error(fun() -> create_binary_binary(<<"foo">>, id(bar)) end, badarg).

test_create_with_binary_size_out_of_range() ->
    expect_error(fun() -> create_binary_binary(<<"foo">>, id(4)) end, badarg).

test_create_with_unsupported_binary_unit() ->
    atom_unsupported(fun() -> create_binary_binary_unit_3(<<"foo">>, id(3)) end).

% Things are very broken here, we get {badmatch, <<16#FFFFFFFF:32>>} but
% this term isn't equal to {badmatch, <<16#FFFFFFFF:32>>}
test_get_with_unsupported_int_unit() ->
    atom_unsupported(
        fun() ->
            R = id(16#FF),
            R = get_integer_big_unsigned_unit_3(id(<<16#FFFFFFFF:32>>), id(8))
        end,
        fun(_) -> true end
    ).

test_get_with_int_little_endian() ->
    expect_equals(1024, get_integer_little_unsigned(<<0, 4, 0, 0>>, 32)).

test_get_with_int_signed() ->
    expect_equals(-1024, get_integer_big_signed(<<255, 255, 252, 0>>, 32)).

test_get_with_unaligned_binary() ->
    atom_unsupported(fun() -> get_int_then_binary(<<1, 2, 3, 4>>, id(4), id(1)) end, fun(T) ->
        T =:= badarg
    end).

create_int_binary_unit_3(Value, Size) ->
    <<Value:Size/integer-big-unit:3>>.

create_int_binary(Value, Size) ->
    <<Value:Size>>.

create_int_binary_little_endian(Value, Size) ->
    <<Value:Size/little>>.

create_int_binary_signed(Value, Size) ->
    <<Value:Size/signed>>.

create_binary_binary(Value, Size) ->
    <<Value:Size/binary>>.

create_binary_binary_unit_3(Value, Size) ->
    <<Value:Size/binary-unit:3>>.

get_integer_big_unsigned(Bin, Size) ->
    <<Value:Size, _Rest/binary>> = Bin,
    Value.

get_integer_big_unsigned_unit_3(Bin, Size) ->
    <<Value:Size, _Rest/binary-unit:3>> = Bin,
    Value.

get_integer_little_unsigned(Bin, Size) ->
    <<Value:Size/little, _Rest/binary>> = Bin,
    Value.

get_integer_big_signed(Bin, Size) ->
    <<Value:Size/signed, _Rest/binary>> = Bin,
    Value.

get_int_then_binary(Bin, IntSize, BinSize) ->
    <<IntValue:IntSize/integer, BinValue:BinSize/binary, _Rest/bitstring>> = Bin,
    {IntValue, BinValue}.

expect_equals(A, A) ->
    ok;
expect_equals(A, B) ->
    throw({not_equal, A, B}).

expect_error(F, Reason) when is_atom(Reason) orelse is_tuple(Reason) ->
    expect_error(F, fun(Tag, Value) -> Tag =:= error andalso Value =:= Reason end);
expect_error(F, ErrorValidator) when is_function(ErrorValidator) ->
    ok =
        try
            F(),
            unexpected
        catch
            T:V ->
                case ErrorValidator(T, V) of
                    false ->
                        erlang:display({T, V}),
                        {got, {T, V}, validator_failed};
                    true ->
                        ok
                end
        end.

atom_unsupported(F) ->
    atom_unsupported(F, fun(Reason) -> Reason =:= unsupported end).

atom_unsupported(F, ExpectedErrorValidator) ->
    R =
        try
            F(),
            ok
        catch
            error:Reason -> Reason
        end,
    case erlang:system_info(machine) of
        "BEAM" ->
            R = ok;
        _ ->
            true = ExpectedErrorValidator(R),
            ok
    end.

test_match_first_integer(<<16#FF:8, Rest/binary>>) ->
    Rest;
test_match_first_integer(<<16#ABCD:16, Rest/binary>>) ->
    Rest;
test_match_first_integer(_) ->
    nope.

test_bs_append(Bin1, Bin2) ->
    <<Bin1/binary, Bin2/binary>>.

test_bs_private_append(Bin) ->
    <<<<Byte:8>> || <<Byte:8>> <= Bin>>.

test_match_clause(
    <<$n:8, FixedBinaryData:4/binary, Rest/binary>>
) ->
    {$n, FixedBinaryData, Rest};
test_match_clause(
    <<$c:8, ChA:32, DiA:16/binary>>
) ->
    {$c, ChA, DiA};
test_match_clause(_) ->
    nope.

test_match_recursive(<<"">> = Empty, Accum) ->
    [Empty | Accum];
test_match_recursive(<<122:8, Rest/binary>>, Accum) ->
    test_match_recursive(Rest, [122 | Accum]);
test_match_recursive(<<119:32, Rest/binary>>, Accum) ->
    test_match_recursive(Rest, [119 | Accum]);
test_match_recursive(_SoFar, _Accum) ->
    nope.

test_match_force_gc(<<ReallyBigBin:1024/binary, Rest/binary>>) ->
    {ReallyBigBin, Rest}.

make_binary(Size) ->
    make_binary(Size, <<"">>).

make_binary(0, Accum) ->
    Accum;
make_binary(Size, Accum) ->
    Byte = Size rem 256,
    make_binary(Size - 1, <<Accum/binary, Byte:8>>).

test_put_match_string(Prefix, Suffix) ->
    Bin = <<$f:8, $o:8, $o:8, Suffix/binary>>,
    <<Prefix:3/binary, $b:8, $a:8, $r:8>> = Bin.

test_skip_bits() ->
    <<"oobar">> = skip_bits(8, <<"foobar">>),
    <<"obar">> = skip_bits(16, <<"foobar">>),
    <<"">> = skip_bits(48, <<"foobar">>),
    ok = expect_error(fun() -> skip_bits(128, <<"foobar">>) end, {badmatch, <<"foobar">>}),
    ok = expect_error(fun() -> skip_bits(1, <<"foobar">>) end, fun skip_bits_unsupported/2),
    ok.

skip_bits_unsupported(Tag, Value) ->
    case erlang:system_info(machine) of
        "BEAM" -> Tag =:= error andalso Value =:= {badmatch, <<"foobar">>};
        % cannot match here, things are broken (and unsupported)
        _ -> true
    end.

skip_bits(Len, Bin) ->
    <<_First:Len, Rest/binary>> = Bin,
    Rest.

test_match_case_type() ->
    foo = match_case_type([foo, bar]),
    $a = match_case_type(<<"abc">>),
    something_else_entirely = match_case_type(blahblah),
    ok.

match_case_type(Term) ->
    case Term of
        [H | _T] ->
            H;
        <<H:8, _/binary>> ->
            H;
        _ ->
            something_else_entirely
    end.

-define(TEST_BINARY_DATA,
    <<241, 131, 104, 2, 100, 0, 4, 99, 97, 108, 108, 104, 2, 104, 3, 100, 0, 3, 106, 111, 101, 100,
        0, 6, 114, 111, 98, 101, 114, 116, 97, 0, 104, 2, 100, 0, 5, 104, 101, 108, 108, 111, 97,
        1>>
).

%% erlfmt-ignore
-define(TEST_LIST_DATA,
    [241, 131, 104, 2, 100, 0, 4, 99, 97, 108, 108, 104, 2, 104, 3, 100, 0, 3, 106, 111, 101,100,
        0, 6, 114, 111, 98, 101, 114, 116, 97, 0, 104, 2, 100, 0, 5, 104, 101, 108, 108, 111, 97,
        1]
).

test_iterate_binary() ->
    ?TEST_LIST_DATA = traverse(id(?TEST_BINARY_DATA), []),
    ok.

traverse(<<"">>, Accum) -> Accum;
traverse(<<H:8, T/binary>>, Accum) -> traverse(T, Accum ++ [H]).

test_large() ->
    X = <<42:1024>>,
    true = id(X) =:= <<42:1024>>,
    ok.

% From OTP22, the sequence 1:1,11:4,3:3 is converted to a string of 1 byte
% OTP 25-26 use OP_BS_CREATE_BIN with STRING
% OTP 22-24 use OP_BS_PUT_STRING
% OTP 21 uses OP_BS_PUT_INTEGER
test_copy_bits_string() ->
    A = id(42),
    X1 = id(0),
    X2 = id(0),
    Y1 = <<A:16/little, X1:7, 1:1, 11:4, 3:3, X2:1>>,
    <<42, 0, 1, 182>> = Y1,
    ok.

id(X) -> X.
