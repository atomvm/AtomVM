% Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

-module(int_atom_conv).

-export([observe/5, existing_atoms/0]).

existing_atoms() ->
    [
        '',
        ok,
        true,
        false,
        badarg,
        utf8,
        unicode,
        latin1,
        lynx_ia_ex_ascii,
        'lynx_ia_ex_0',
        '0',
        'éÿ',
        'アトム',
        '€',
        '🌑',
        'lynx_ia_ex_255_zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'
    ].

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

duplicate_list(0, _Elem, Acc) ->
    Acc;
duplicate_list(N, Elem, Acc) ->
    duplicate_list(N - 1, Elem, [Elem | Acc]).

duplicate_bin(0, _Chunk, Acc) ->
    Acc;
duplicate_bin(N, Chunk, Acc) ->
    duplicate_bin(N - 1, Chunk, <<Chunk/binary, Acc/binary>>).

append_tail([], Tail) ->
    Tail;
append_tail([H | T], Tail) ->
    [H | append_tail(T, Tail)].

observe(Op, Mode, Offset, Input0, Extra0) ->
    _ = existing_atoms(),
    {CopiedIn, Extra} = binary_to_term(term_to_binary({Input0, Extra0})),
    Input =
        case Mode of
            copy ->
                CopiedIn;
            bit_slice ->
                slice_leaves(CopiedIn, Offset);
            rep_list ->
                case CopiedIn of
                    {Prefix, Count, Elem, Suffix} ->
                        Prefix ++ duplicate_list(Count, Elem, Suffix);
                    {Prefix, Count, Elem, Suffix, Tail} ->
                        Prefix ++ duplicate_list(Count, Elem, append_tail(Suffix, Tail))
                end;
            rep_bin ->
                {Prefix, Count, Chunk, Suffix} = CopiedIn,
                Body = duplicate_bin(Count, Chunk, <<>>),
                <<Prefix/binary, Body/binary, Suffix/binary>>;
            rep_atom ->
                {Count, Cp, Suffix} = CopiedIn,
                list_to_atom(duplicate_list(Count, Cp, Suffix))
        end,
    Captured = {captured, Input, Offset, Extra, <<5:3>>},
    true = erlang:garbage_collect(),
    Outcome =
        try
            case Op of
                i2l_1 -> erlang:integer_to_list(Input);
                i2l_2 -> erlang:integer_to_list(Input, Extra);
                i2b_1 -> erlang:integer_to_binary(Input);
                i2b_2 -> erlang:integer_to_binary(Input, Extra);
                l2i_1 -> erlang:list_to_integer(Input);
                l2i_2 -> erlang:list_to_integer(Input, Extra);
                b2i_1 -> erlang:binary_to_integer(Input);
                b2i_2 -> erlang:binary_to_integer(Input, Extra);
                a2l_1 -> erlang:atom_to_list(Input);
                a2b_1 -> erlang:atom_to_binary(Input);
                a2b_2 -> erlang:atom_to_binary(Input, Extra);
                l2a_1 -> erlang:list_to_atom(Input);
                l2ea_1 -> erlang:list_to_existing_atom(Input);
                b2a_1 -> erlang:binary_to_atom(Input);
                b2a_2 -> erlang:binary_to_atom(Input, Extra);
                b2ea_1 -> erlang:binary_to_existing_atom(Input);
                b2ea_2 -> erlang:binary_to_existing_atom(Input, Extra)
            end
        of
            Res -> {ok, Res}
        catch
            Class:Reason -> {exception, Class, Reason}
        end,
    true = erlang:garbage_collect(),
    {Outcome, Input, Offset, Extra, Captured}.
