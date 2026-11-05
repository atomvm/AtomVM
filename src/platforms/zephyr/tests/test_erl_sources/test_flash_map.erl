%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M <petermm@gmail.com>
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_flash_map).
-export([start/0]).

start() ->
    Areas = zephyr:flash_list(),
    true = is_list(Areas),
    true = Areas =/= [],
    ok = lists:foreach(fun assert_area/1, Areas),
    ok = test_storage(Areas),
    Scratch = find_labeled(Areas, <<"image-scratch">>),
    case Scratch of
        undefined ->
            ok;
        #{id := Id, size := Size} when Size >= 4096 ->
            ok = test_read_write_erase(Id, Size)
    end,
    ok.

assert_area(#{id := Id, offset := Offset, size := Size} = Area) ->
    true = is_integer(Id) andalso Id >= 0,
    true = is_integer(Offset) andalso Offset >= 0,
    true = is_integer(Size) andalso Size > 0,
    case maps:get(label, Area, undefined) of
        undefined ->
            ok;
        Label when is_binary(Label) ->
            ok
    end.

test_storage(Areas) ->
    case find_labeled(Areas, <<"storage">>) of
        undefined ->
            ok;
        #{id := Id, size := Size} when Size > 0 ->
            {ok, Bin} = zephyr:flash_read(<<"storage">>, 0, 1),
            true = is_binary(Bin) andalso byte_size(Bin) =:= 1,
            {ok, Bin} = zephyr:flash_read(Id, 0, 1),
            ok
    end.

find_labeled(Areas, Label) ->
    case
        lists:filter(
            fun
                (#{label := AreaLabel}) -> AreaLabel =:= Label;
                (_) -> false
            end,
            Areas
        )
    of
        [Area | _] ->
            Area;
        [] ->
            undefined
    end.

test_read_write_erase(Id, Size) ->
    ok = zephyr:flash_erase(<<"image-scratch">>, 0, 4096),
    ok = zephyr:flash_write(<<"image-scratch">>, 0, <<"hello">>),
    {ok, <<"hello">>} = zephyr:flash_read(<<"image-scratch">>, 0, 5),
    {ok, <<"hello">>} = zephyr:flash_read(Id, 0, 5),
    ok = maybe_assert_mmap(<<"hello">>),
    ok = test_mmap_bounds(Size),
    ok = test_mmap_lifetime(<<"hello">>),
    ok = zephyr:flash_erase(Id, 0),
    {ok, <<16#FF, 16#FF, 16#FF, 16#FF, 16#FF>>} = zephyr:flash_read(Id, 0, 5),
    ok = zephyr:flash_write(Id, 0, <<"world">>),
    {ok, <<"world">>} = zephyr:flash_read(<<"image-scratch">>, 0, 5),
    ok = maybe_assert_mmap(<<"world">>),
    {error, not_found} = zephyr:flash_read(<<"no-such-area">>, 0, 1),
    ok.

maybe_assert_mmap(Expected) ->
    case zephyr:flash_mmap(<<"image-scratch">>, 0, byte_size(Expected)) of
        {ok, Expected} ->
            ok;
        {error, not_supported} ->
            ok
    end.

test_mmap_bounds(Size) ->
    case zephyr:flash_mmap(<<"image-scratch">>, Size - 1, 2) of
        {error, io_error} ->
            {ok, <<_>>} = zephyr:flash_mmap(<<"image-scratch">>, Size - 1, 1),
            ok;
        {error, not_supported} ->
            ok
    end.

test_mmap_lifetime(Expected) ->
    case map_pair(Expected) of
        not_supported ->
            ok;
        Second ->
            erlang:garbage_collect(),
            Expected = Second,
            ok
    end.

map_pair(Expected) ->
    case zephyr:flash_mmap(<<"image-scratch">>, 0, byte_size(Expected)) of
        {error, not_supported} ->
            not_supported;
        {ok, First} ->
            {ok, Second} = zephyr:flash_mmap(<<"image-scratch">>, 0, byte_size(Expected)),
            Expected = First,
            Expected = Second,
            Second
    end.