#!/usr/bin/env escript
%
% This file is part of AtomVM.
%
% Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

-module(uf2tool).

-export([main/1]).
-mode(compile).

main([]) ->
    io:format("UF2 tool for AtomVM usage on Pico\n");
main(["-h"]) ->
    usage();
main(["help"]) ->
    usage();
main(["join", "-o", Output | Sources]) when length(Sources) > 1 ->
    uf2join(Output, Sources);
main(["create", "-o", Output, "-s", StartAddrStr, Image]) ->
    StartAddr = parse_addr(StartAddrStr),
    uf2create(Output, StartAddr, Image);
main(_) ->
    io:format("Syntax error\n"),
    usage(),
    erlang:halt(2).

usage() ->
    io:format("UF2 Tool, AtomVM version @ATOMVM_VERSION@\n\n"),
    io:format("Usage:\n"),
    io:format("  uf2tool help | -h\n"),
    io:format("    Display this message\n\n"),
    io:format("  uf2tool join -o combined.uf2 first.uf2 second.uf2...\n"),
    io:format("    Join two or more UF2 binaries\n\n"),
    io:format("  uf2tool create -o new.uf2 -s start_addr image.avm\n"),
    io:format("    Create a UF2 image from a binary file (image.avm), suitable for the Pico\n"),
    ok.

parse_addr("0x" ++ AddrHex) ->
    list_to_integer(AddrHex, 16);
parse_addr("16#" ++ AddrHex) ->
    list_to_integer(AddrHex, 16);
parse_addr(AddrDec) ->
    list_to_integer(AddrDec).

%%% UF2 defines
-define(UF2_MAGIC_START0, 16#0A324655).
-define(UF2_MAGIC_START1, 16#9E5D5157).
-define(UF2_MAGIC_END, 16#0AB16F30).

-define(UF2_FLAG_FAMILY_ID_PRESENT, 16#00002000).

%%% Pico defines
-define(UF2_PICO_FLAGS, ?UF2_FLAG_FAMILY_ID_PRESENT).
-define(UF2_PICO_PAGE_SIZE, 256).
-define(UF2_PICO_FAMILY_ID, 16#E48BFF56).

%%%

uf2join(OutputPath, Sources) ->
    SourceBins = [Bin || {ok, Bin} <- [file:read_file(Source) || Source <- Sources]],
    BlocksCount = lists:sum([byte_size(SourceBin) || SourceBin <- SourceBins]) div 512,
    {BlocksCount, OutputBinsLR} = lists:foldl(
        fun(SourceBin, {StartBlock, Acc}) ->
            {NewBlockStart, RewrittenBin} = rewrite_block_indices(
                StartBlock, BlocksCount, SourceBin
            ),
            {NewBlockStart, [RewrittenBin | Acc]}
        end,
        {0, []},
        SourceBins
    ),
    ok = file:write_file(OutputPath, lists:reverse(OutputBinsLR)).

rewrite_block_indices(BlockIndex, BlocksCount, Bin) ->
    rewrite_block_indices0(BlockIndex, BlocksCount, Bin, []).

rewrite_block_indices0(LastBlock, _BlocksCount, <<>>, Acc) ->
    {LastBlock, lists:reverse(Acc)};
rewrite_block_indices0(BlockIndex, BlocksCount, <<Page:512/binary, Tail/binary>>, Acc) ->
    <<
        ?UF2_MAGIC_START0:32/little,
        ?UF2_MAGIC_START1:32/little,
        Flags:32/little,
        TargetAddr:32/little,
        PageSize:32/little,
        _BlockNo:32/little,
        _NumBlocks:32/little,
        FamilyIdOrFileSize:32/little,
        Data:476/binary,
        ?UF2_MAGIC_END:32/little
    >> = Page,
    Rewritten = <<
        ?UF2_MAGIC_START0:32/little,
        ?UF2_MAGIC_START1:32/little,
        Flags:32/little,
        TargetAddr:32/little,
        PageSize:32/little,
        BlockIndex:32/little,
        BlocksCount:32/little,
        FamilyIdOrFileSize:32/little,
        Data:476/binary,
        ?UF2_MAGIC_END:32/little
    >>,
    rewrite_block_indices0(BlockIndex + 1, BlocksCount, Tail, [Rewritten | Acc]).

uf2create(OutputPath, StartAddr, ImagePath) ->
    {ok, ImageBin} = file:read_file(ImagePath),
    BlocksCount0 = byte_size(ImageBin) div ?UF2_PICO_PAGE_SIZE,
    BlocksCount =
        BlocksCount0 +
            if
                byte_size(ImageBin) rem ?UF2_PICO_PAGE_SIZE =:= 0 -> 0;
                true -> 1
            end,
    OutputBin = uf2create0(0, BlocksCount, StartAddr, ImageBin, []),
    ok = file:write_file(OutputPath, OutputBin).

uf2create0(_BlockIndex, _BlocksCount, _BaseAddr, <<>>, Acc) ->
    lists:reverse(Acc);
uf2create0(BlockIndex, BlocksCount, BaseAddr, ImageBin, Acc) ->
    {PageBin, Tail} =
        if
            byte_size(ImageBin) >= ?UF2_PICO_PAGE_SIZE ->
                split_binary(ImageBin, ?UF2_PICO_PAGE_SIZE);
            true ->
                {ImageBin, <<>>}
        end,
    PaddedData = pad_binary(PageBin, 476),
    Block = [
        <<
            ?UF2_MAGIC_START0:32/little,
            ?UF2_MAGIC_START1:32/little,
            ?UF2_PICO_FLAGS:32/little,
            BaseAddr:32/little,
            ?UF2_PICO_PAGE_SIZE:32/little,
            BlockIndex:32/little,
            BlocksCount:32/little,
            ?UF2_PICO_FAMILY_ID:32/little
        >>,
        PaddedData,
        <<?UF2_MAGIC_END:32/little>>
    ],
    uf2create0(BlockIndex + 1, BlocksCount, BaseAddr + ?UF2_PICO_PAGE_SIZE, Tail, [Block | Acc]).

pad_binary(Bin, Len) ->
    PadCount = Len - byte_size(Bin),
    Pad = binary:copy(<<0>>, PadCount),
    [Bin, Pad].
