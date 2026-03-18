%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%% Force the compiler to use bs_test_unit opcode instead of the
%% newer bs_match opcode (OTP 25+). On older OTP this is ignored.
%% On OTP 29+, no_bs_match was removed but no_ssa_opt_bs_ensure
%% prevents the SSA pass from converting bs_test_unit to bs_match
%% with ensure_at_least.
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE =< 28).
-compile([no_bs_match]).
-endif.
-if(?OTP_RELEASE >= 29).
-compile([no_ssa_opt_bs_ensure]).
-endif.
-endif.

-module(test_op_bs_test_unit).

-export([start/0]).

start() ->
    ok = test_byte_aligned(),
    ok = test_unit_16(),
    0.

test_byte_aligned() ->
    %% A byte-aligned binary should pass bs_test_unit with unit=8
    <<1, 2, 3, 4>> = get_tail_if_byte_aligned(id(<<1, 2, 3, 4>>)),
    <<>> = get_tail_if_byte_aligned(id(<<>>)),
    <<2, 3>> = get_tail_if_byte_aligned(id(<<2, 3>>)),
    ok.

test_unit_16() ->
    %% After skipping 8 bits, remaining must be 16-bit aligned
    %% 5 bytes = 40 bits, skip 8 => 32 remaining, 32 rem 16 = 0 => ok
    <<2, 3, 4, 5>> = get_tail_unit_16(id(<<1, 2, 3, 4, 5>>)),
    %% 3 bytes = 24 bits, skip 8 => 16 remaining, 16 rem 16 = 0 => ok
    <<2, 3>> = get_tail_unit_16(id(<<1, 2, 3>>)),
    %% 2 bytes = 16 bits, skip 8 => 8 remaining, 8 rem 16 = 8 => fail
    error = get_tail_unit_16(id(<<1, 2>>)),
    %% 4 bytes = 32 bits, skip 8 => 24 remaining, 24 rem 16 = 8 => fail
    error = get_tail_unit_16(id(<<1, 2, 3, 4>>)),
    ok.

%% Start a match, verify the remaining bits are byte-aligned (unit=8),
%% then return the tail. Returns 'error' on failure.
get_tail_if_byte_aligned(Bin) ->
    case Bin of
        <<_/binary-unit:8>> -> Bin;
        _ -> error
    end.

%% Start a match, skip 8 bits, verify remaining bits are 16-bit aligned,
%% then return the tail. Returns 'error' on failure.
get_tail_unit_16(<<_:8, Rest/binary>>) ->
    case Rest of
        <<_/binary-unit:16>> -> Rest;
        _ -> error
    end;
get_tail_unit_16(_) ->
    error.

id(X) -> X.
