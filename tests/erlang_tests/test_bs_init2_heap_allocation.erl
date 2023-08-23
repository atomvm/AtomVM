%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_bs_init2_heap_allocation).

-export([start/0]).

start() ->
    Display = self(),
    disp(Display, 16#00000000),
    disp(Display, 16#000000FF),
    receive
        [{text, 10, 16#000000FF, <<"Test 255">>}] -> ok
    end,
    0.

%% Because of integer_to_binary, the size of the binary is computed
%% with byte_size which is a gc bif. As a consequence, put_tuple2 and put_list
%% are not preceded by a test_heap opcode. Instead, the amount of required
%% memory is encoded in bs_init2.
%% This code crashed when compiled with OTP24 before bs_init2 actually ensured
%% the required amount of words.
disp(Display, Num) ->
    Bin = integer_to_binary(Num),
    Scene = [
        {text, 10, Num, <<"Test ", Bin/binary>>}
    ],
    Display ! Scene.
