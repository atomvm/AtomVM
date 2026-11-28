% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(unicode_conv).

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

observe(Op, Mode, Offset, Input0, Encs) ->
    {Copied, Enc} = binary_to_term(term_to_binary({Input0, Encs})),
    Input =
        case Mode of
            bit_slice -> slice_leaves(Copied, Offset);
            deep -> build_deep(Offset, Copied);
            copy -> Copied
        end,
    Captured = {captured, Input, Offset, Enc, <<5:3>>},
    true = erlang:garbage_collect(),
    Outcome =
        try
            case Op of
                to_list_1 ->
                    unicode:characters_to_list(Input);
                to_list_2 ->
                    unicode:characters_to_list(Input, Enc);
                to_binary_1 ->
                    unicode:characters_to_binary(Input);
                to_binary_2 ->
                    unicode:characters_to_binary(Input, Enc);
                to_binary_3 ->
                    {InEnc, OutEnc} = Enc,
                    unicode:characters_to_binary(Input, InEnc, OutEnc)
            end
        of
            {error, Conv, Rest} when is_binary(Input) andalso is_binary(Rest) ->
                {error_bin, Conv, Rest};
            {incomplete, Conv, Rest} when is_binary(Input) andalso is_binary(Rest) ->
                {incomplete_bin, Conv, Rest};
            {error, Conv, Rest} when is_binary(Rest) orelse is_list(Rest) ->
                {error_list, Conv};
            {incomplete, Conv, Rest} when is_binary(Rest) orelse is_list(Rest) ->
                {incomplete_list, Conv};
            Res ->
                {ok, Res}
        catch
            Class:Reason -> {exception, Class, Reason}
        end,
    true = erlang:garbage_collect(),
    {Outcome, Input, Offset, Enc, Captured}.
