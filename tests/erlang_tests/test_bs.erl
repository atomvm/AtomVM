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

-export([start/0, id/1, join/2]).

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
    ok = test_bs_match_string_unaligned(),

    test_match_case_type(),

    ok = test_iterate_binary(),

    ok = test_large(),

    ok = test_copy_bits_string(),
    ok = test_bs_match_string_select(),

    ok = test_bs_skip_bits2_little(),

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
    <<2, 1>> = create_int_binary_little_endian(16#0102, 16),
    <<254, 255>> = create_int_binary_little_endian(16#FFFE, 16),
    <<4, 3, 2, 1>> = create_int_binary_little_endian(16#01020304, 32),
    <<252, 253, 254, 255>> = create_int_binary_little_endian(16#FFFEFDFC, 32),
    <<0>> = create_int_binary_little_endian(1024, 8),

    <<0, 2, 1>> = create_int_binary_little_endian(8, 16#0102, 16),
    <<0, 254, 255>> = create_int_binary_little_endian(8, 16#FFFE, 16),
    <<0, 4, 3, 2, 1>> = create_int_binary_little_endian(8, 16#01020304, 32),
    <<0, 252, 253, 254, 255>> = create_int_binary_little_endian(8, 16#FFFEFDFC, 32),

    <<0, 0, 2, 1>> = create_int_binary_little_endian(16, 16#0102, 16),
    <<0, 0, 254, 255>> = create_int_binary_little_endian(16, 16#FFFE, 16),
    <<0, 0, 4, 3, 2, 1>> = create_int_binary_little_endian(16, 16#01020304, 32),
    <<0, 0, 252, 253, 254, 255>> = create_int_binary_little_endian(16, 16#FFFEFDFC, 32),

    <<0, 0, 0, 2, 1>> = create_int_binary_little_endian(24, 16#0102, 16),
    <<0, 0, 0, 254, 255>> = create_int_binary_little_endian(24, 16#FFFE, 16),
    <<0, 0, 0, 4, 3, 2, 1>> = create_int_binary_little_endian(24, 16#01020304, 32),
    <<0, 0, 0, 252, 253, 254, 255>> = create_int_binary_little_endian(24, 16#FFFEFDFC, 32),
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
    Bin1 = id(<<255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244>>),
    Bin2 = id(<<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>),
    16#FEFF = get_integer_little_unsigned(id(Bin1), 16),
    16#0100 = get_integer_little_unsigned(id(Bin2), 16),
    16#FCFDFEFF = get_integer_little_unsigned(id(Bin1), 32),
    16#03020100 = get_integer_little_unsigned(id(Bin2), 32),
    %   16#F8F9FAFBFCFDFEFF = get_integer_little_unsigned(id(Bin1), 64),
    %   Even this fails as well until we have proper bigint support as we can't
    %   represent 16#F8F9FAFBFCFDFEFF unsigned
    %   X = get_integer_little_unsigned(id(Bin1), 64),
    %   16#F8F9FAFB = X bsr 32,
    %   16#FCFDFEFF = X band 16#FFFFFFFF,
    16#0706050403020100 = get_integer_little_unsigned(id(Bin2), 64),

    16#FDFE = get_integer_little_unsigned(8, id(Bin1), 16),
    16#0201 = get_integer_little_unsigned(8, id(Bin2), 16),
    16#FBFCFDFE = get_integer_little_unsigned(8, id(Bin1), 32),
    16#04030201 = get_integer_little_unsigned(8, id(Bin2), 32),
    %   16#F7F8F9FAFBFCFDFE = get_integer_little_unsigned(8, id(Bin1), 64),
    16#0807060504030201 = get_integer_little_unsigned(8, id(Bin2), 64),

    16#FCFD = get_integer_little_unsigned(16, id(Bin1), 16),
    16#0302 = get_integer_little_unsigned(16, id(Bin2), 16),
    16#FAFBFCFD = get_integer_little_unsigned(16, id(Bin1), 32),
    16#05040302 = get_integer_little_unsigned(16, id(Bin2), 32),
    %   16#F6F7F8F9FAFBFCFD = get_integer_little_unsigned(16, id(Bin1), 64),
    16#0908070605040302 = get_integer_little_unsigned(16, id(Bin2), 64),

    16#FBFC = get_integer_little_unsigned(24, id(Bin1), 16),
    16#0403 = get_integer_little_unsigned(24, id(Bin2), 16),
    16#F9FAFBFC = get_integer_little_unsigned(24, id(Bin1), 32),
    16#06050403 = get_integer_little_unsigned(24, id(Bin2), 32),
    %   16#F5F6F7F8F9FAFBFC = get_integer_little_unsigned(24, id(Bin1), 64),
    16#0A09080706050403 = get_integer_little_unsigned(24, id(Bin2), 64),

    ok.

test_get_with_int_signed() ->
    Bin1 = id(<<255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244>>),
    Bin2 = id(<<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>),
    Bin3 = id(<<128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139>>),
    -16#0002 = get_integer_big_signed(id(Bin1), 16),
    16#0001 = get_integer_big_signed(id(Bin2), 16),
    -16#00010204 = get_integer_big_signed(id(Bin1), 32),
    16#00010203 = get_integer_big_signed(id(Bin2), 32),
    -16#01020304050608 = get_integer_big_signed(id(Bin1), 64),
    16#01020304050607 = get_integer_big_signed(id(Bin2), 64),
    -16#7F7E7D7C7B7A7979 = get_integer_big_signed(id(Bin3), 64),

    -16#0103 = get_integer_big_signed(8, id(Bin1), 16),
    16#0102 = get_integer_big_signed(8, id(Bin2), 16),
    -16#01020305 = get_integer_big_signed(8, id(Bin1), 32),
    16#01020304 = get_integer_big_signed(8, id(Bin2), 32),
    -16#0102030405060709 = get_integer_big_signed(8, id(Bin1), 64),
    16#0102030405060708 = get_integer_big_signed(8, id(Bin2), 64),
    -16#7E7D7C7B7A797878 = get_integer_big_signed(8, id(Bin3), 64),

    -16#0204 = get_integer_big_signed(16, id(Bin1), 16),
    16#0203 = get_integer_big_signed(16, id(Bin2), 16),
    -16#02030406 = get_integer_big_signed(16, id(Bin1), 32),
    16#02030405 = get_integer_big_signed(16, id(Bin2), 32),
    -16#020304050607080A = get_integer_big_signed(16, id(Bin1), 64),
    16#0203040506070809 = get_integer_big_signed(16, id(Bin2), 64),
    -16#7D7C7B7A79787777 = get_integer_big_signed(16, id(Bin3), 64),

    -16#0305 = get_integer_big_signed(24, id(Bin1), 16),
    16#0304 = get_integer_big_signed(24, id(Bin2), 16),
    -16#03040507 = get_integer_big_signed(24, id(Bin1), 32),
    16#03040506 = get_integer_big_signed(24, id(Bin2), 32),
    -16#030405060708090B = get_integer_big_signed(24, id(Bin1), 64),
    16#030405060708090A = get_integer_big_signed(24, id(Bin2), 64),
    -16#7C7B7A7978777676 = get_integer_big_signed(24, id(Bin3), 64),

    ok.

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

create_int_binary_little_endian(Skip, Value, Size) ->
    <<0:Skip, Value:Size/little>>.

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

get_integer_little_unsigned(Skip, Bin, Size) ->
    <<_:Skip, Value:Size/little, _Rest/binary>> = Bin,
    Value.

get_integer_big_signed(Bin, Size) ->
    <<Value:Size/signed, _Rest/binary>> = Bin,
    Value.

get_integer_big_signed(Skip, Bin, Size) ->
    <<_:Skip, Value:Size/signed, _Rest/binary>> = Bin,
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

test_bs_match_string_unaligned() ->
    <<0:1, _:3, 42:7, _:5, 42>> = id(<<0:3, 42, 0:5, 42>>),
    <<0:1, _:3, 42:12, _:8, 42>> = id(<<0, 42, 0, 42>>),
    ok = expect_error(
        fun() -> <<0:1, _:4, 42:12, 0:7>> = id(<<0:5, 42, 0:3>>) end, {badmatch, <<1, 80>>}
    ),
    ok.

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

% With OTP27, this generates the following code:
%    {test,bs_start_match3,{f,197},1,[{x,0}],{x,0}}.
%    {bs_match,{f,197},
%              {x,0},
%              {commands,[{ensure_at_least,16,1},
%                         {integer,1,{literal,[]},16,1,{x,1}}]}}.
%    {bs_get_position,{x,0},{x,2},2}.
%    {select_val,{tr,{x,1},{t_integer,{0,65535}}},
%                {f,197},
%                {list,[{integer,24940},
%                       {f,196},
%                       {integer,28271},
%                       {f,195},
%                       {integer,28523},
%                       {f,193}]}}.
%  {label,193}.
%    {test,bs_match_string,{f,194},[{x,0},104,{string,<<"_simultaneous">>}]}.
%    {bs_match,{f,194},{x,0},{commands,[{ensure_exactly,0}]}}.
%    {move,{atom,ok},{x,0}}.
%    {jump,{f,198}}.
%
% We ensure here (using valgrind) that bs_match_string doesn't try to compare
% beyond heap where <<"okay">> is.
test_bs_match_string_select() ->
    R = <<"ok">>,
    L = <<"ay">>,
    Z =
        case join(R, L) of
            <<"ok">> -> nok_ok;
            <<"ok_simultaneous">> -> nok_simultaneous;
            <<"alive">> -> nok_alive;
            _Other -> ok
        end,
    id(Z).

% With OTP25 and lower, this generates bs_skip_init2 with flags equal to 2 (little)
% More recent versions of OTP use bs_match
test_bs_skip_bits2_little() ->
    ok = check_x86_64_jt(id(<<16#e9, 0:32>>)).

check_x86_64_jt(<<>>) -> ok;
check_x86_64_jt(<<16#e9, _Offset:32/little, Tail/binary>>) -> check_x86_64_jt(Tail);
check_x86_64_jt(Bin) -> {unexpected, Bin}.

id(X) -> X.

join(X, Y) ->
    <<X/binary, Y/binary>>.
