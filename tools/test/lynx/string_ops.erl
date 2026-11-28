% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(string_ops).

-export([observe/5]).

slice_leaves(B, Offset) when is_bitstring(B) ->
    Size = bit_size(B),
    Padding = (8 - (Offset + Size) rem 8) rem 8,
    Parent = <<201:640, 5:Offset, B/bitstring, 0:Padding, 77>>,
    Start = 640 + Offset,
    <<_:Start, Slice:Size/bitstring, _/bitstring>> = Parent,
    Slice;
slice_leaves([H | T], Offset) ->
    [slice_leaves(H, Offset) | slice_leaves(T, Offset)];
slice_leaves({A, B}, Offset) ->
    {slice_leaves(A, Offset), slice_leaves(B, Offset)};
slice_leaves(Other, _Offset) ->
    Other.

build_deep(0, Leaf) ->
    Leaf;
build_deep(N, Leaf) ->
    case N rem 3 of
        0 -> [build_deep(N - 1, Leaf)];
        1 -> [<<>>, build_deep(N - 1, Leaf), []];
        2 -> [build_deep(N - 1, Leaf) | <<>>]
    end.

observe(Op, Mode, Offset, Input0, Extra0) ->
    {CopiedIn, CopiedExtra} = binary_to_term(term_to_binary({Input0, Extra0})),
    {Input, Extra} =
        case Mode of
            bit_slice ->
                {slice_leaves(CopiedIn, Offset), slice_leaves(CopiedExtra, Offset)};
            deep ->
                {build_deep(Offset, CopiedIn), CopiedExtra};
            copy ->
                {CopiedIn, CopiedExtra}
        end,
    Captured = {captured, Input, Offset, Extra, <<5:3>>},
    true = erlang:garbage_collect(),
    Outcome =
        try
            case Op of
                to_upper_1 ->
                    string:to_upper(Input);
                to_lower_1 ->
                    string:to_lower(Input);
                split_2 ->
                    string:split(Input, Extra);
                split_3 ->
                    {Pat, Where} = Extra,
                    string:split(Input, Pat, Where);
                trim_1 ->
                    string:trim(Input);
                trim_2 ->
                    string:trim(Input, Extra);
                find_2 ->
                    string:find(Input, Extra);
                find_3 ->
                    {Pat, Dir} = Extra,
                    string:find(Input, Pat, Dir);
                length_1 ->
                    string:length(Input);
                to_integer_1 ->
                    string:to_integer(Input)
            end
        of
            Res -> {ok, Res}
        catch
            Class:Reason -> {exception, Class, Reason}
        end,
    true = erlang:garbage_collect(),
    {Outcome, Input, Offset, Extra, Captured}.
