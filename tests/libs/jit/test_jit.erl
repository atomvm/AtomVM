%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(test_jit).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").

-define(CODE_CHUNK_0,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 177, 0, 0, 0, 7, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 0,
        1, 32, 64, 50, 3, 19, 1, 48, 153, 0, 2, 18, 66, 0, 1, 64, 64, 18, 3, 78, 16, 0, 1, 80, 153,
        0, 2, 18, 66, 16, 1, 96, 64, 3, 19, 64, 18, 3, 78, 32, 16, 3>>
).

compile_minimal_x86_64_test() ->
    Stream0 = jit_stream_binary:new(),
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),
    {_Lines, Stream3} = jit:compile(?CODE_CHUNK_0, jit_x86_64, Stream2),
    Stream4 = jit_x86_64:stream(Stream3),
    <<12:32, ?JIT_FORMAT_VERSION:16, 1:16, ?JIT_ARCH_X86_64:16, ?JIT_VARIANT_PIC:16, 0:32,
        Code/binary>> = Stream4,
    ok = file:write_file("dump.bin", Code),
    ok = file:write_file("avmN_chunk.bin", Stream4),
    ok.
