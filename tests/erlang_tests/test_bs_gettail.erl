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

-module(test_bs_gettail).
-export([start/0, id/1]).

%% Matching-only (get_tail) validation: byte-aligned construction, then match a
%% non-byte-aligned prefix capturing the remainder as a bitstring, then re-match.
start() ->
    Bin = <<(id(16#12345678)):32>>,
    <<I:31, V/bits>> = Bin,
    152709948 = I,
    31 = bit_size(Bin) - bit_size(V),
    1 = bit_size(V),
    false = is_binary(V),
    true = is_bitstring(V),
    <<0:1>> = V,
    Bin2 = <<(id(16#12345679)):32>>,
    <<I:31, W/bits>> = Bin2,
    false = is_binary(W),
    true = is_bitstring(W),
    <<1:1>> = W,
    0.

id(X) -> X.
