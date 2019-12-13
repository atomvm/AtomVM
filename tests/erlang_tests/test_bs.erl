-module(test_bs).

-export([start/0]).

start() ->
    test_pack_small_ints({2, 61, 20}, <<23,180>>),
    test_pack_integer_big_endian(1024, 32, <<0,0,4,0>>),
    IntegersAndBinaries = test_pack_integers_and_binaries(16#F, 16#2, <<"fubar">>, <<"Haddock's Eyes">>, <<0,0,0,15,0,2,102,117,98,97,114,72,97,100>>),
    test_unpack_integers_and_binaries(IntegersAndBinaries, 16#F, 16#2, <<"fubar">>, <<"Had">>),

    error = test_create_with_invalid_int_value(),
    error = test_create_with_invalid_int_size(),
    error = test_create_with_unsupported_int_unit(),
    error = test_create_with_unaligned_int_size(),
    error = test_create_with_int_little_endian(),
    error = test_create_with_int_signed(),
    error = test_create_with_invalid_binary_value(),
    error = test_create_with_invalid_binary_size(),
    error = test_create_with_binary_size_out_of_range(),
    error = test_create_with_invalid_binary_unit(),

    15 = get_integer_big_unsigned(<<16#F>>, 8),
    128 = get_integer_big_unsigned(<<16#80>>, 8),
    4404 = get_integer_big_unsigned(<<0,0,17,52>>, 32),

    error = test_get_with_invalid_int_value(),
    error = test_get_with_invalid_int_size(),
    error = test_get_with_unsupported_int_unit(),
    error = test_get_with_int_little_endian(),
    error = test_get_with_int_signed(),
    error = test_get_with_invalid_binary_value(),
    error = test_get_with_invalid_binary_size(),
    error = test_get_with_unaligned_binary(),

    0.

test_pack_small_ints({A, B, C}, Expect) ->
    Expect = <<A:5, B:6, C:5>>,
    Expect.

test_pack_integer_big_endian(Int, Size, Expect) ->
    Expect = <<Int:Size>>,
    Expect.

test_pack_integers_and_binaries(Int1, Int2, Bin1, Bin2, Expect) ->
    Bin = <<Int1:32/big, Int2:16, Bin1/binary, Bin2:3/binary >>,
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
    expect_error(fun() -> create_int_binary(foo, 32) end, badarg).

test_create_with_invalid_int_size() ->
    expect_error(fun() -> create_int_binary(16#F, bar) end, badarg).

test_create_with_unsupported_int_unit() ->
    expect_error(fun() -> create_int_binary_unit_3(16#F, 32) end, unsupported).

test_create_with_unaligned_int_size() ->
    expect_error(fun() -> create_int_binary(16#FFFF, 28) end, unsupported).

test_create_with_int_little_endian() ->
    expect_error(fun() -> create_int_binary_little_endian(16#FFFF, 32) end, unsupported).

test_create_with_int_signed() ->
    expect_error(fun() -> create_int_binary_signed(16#FFFF, 32) end, unsupported).

test_create_with_invalid_binary_value() ->
    expect_error(fun() -> create_binary_binary(foo, 32) end, badarg).

test_create_with_invalid_binary_size() ->
    expect_error(fun() -> create_binary_binary(<<"foo">>, bar) end, badarg).

test_create_with_binary_size_out_of_range() ->
    expect_error(fun() -> create_binary_binary(<<"foo">>, 4) end, badarg).

test_create_with_invalid_binary_unit() ->
    expect_error(fun() -> create_binary_binary_unit_3(<<"foo">>, 3) end, unsupported).



test_get_with_invalid_int_value() ->
    expect_error(fun() -> get_integer_big_unsigned(foo, 32) end, badarg).

test_get_with_invalid_int_size() ->
    expect_error(fun() -> get_integer_big_unsigned(16#F, bar) end, badarg).

test_get_with_unsupported_int_unit() ->
    expect_error(fun() -> get_integer_big_unsigned_unit_3(16#F, 32) end, unsupported).

test_get_with_int_little_endian() ->
    expect_error(fun() -> get_integer_little_unsigned(16#FFFF, 32) end, unsupported).

test_get_with_int_signed() ->
    expect_error(fun() -> get_integer_big_signed(16#FFFF, 32) end, unsupported).

test_get_with_invalid_binary_value() ->
    expect_error(fun() -> get_binary_binary(foo, 32) end, badarg).

test_get_with_invalid_binary_size() ->
    expect_error(fun() -> get_binary_binary(<<"foo">>, bar) end, badarg).

test_get_with_unaligned_binary() ->
    expect_error(fun() -> get_int_then_binary(<<1,2,3,4>>, 4, 1) end, badarg).


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

get_binary_binary(Bin, Size) ->
    <<Value:Size/binary, _Rest/binary>> = Bin,
    Value.

get_int_then_binary(Bin, IntSize, BinSize) ->
    <<IntValue:IntSize/integer, BinValue:BinSize/binary, _Rest/binary>> = Bin,
    {IntValue, BinValue}.

expect_error(F, _Reason) ->
    try
        F(), ok
    catch
        _E:_R ->
            %% TODO E doesn't seem to match error and R doesn't seem to match Reason
            %% even through they display the same
            %% erlang:display({E, R}),
            %% E = error,
            %% R = Reason,
            error
    end.
