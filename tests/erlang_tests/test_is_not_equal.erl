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

-module(test_is_not_equal).

-export([start/0]).

start() ->
    %% Test the is_ne opcode (OP_IS_NOT_EQUAL, opcode 42) via assembly.
    %% This opcode performs loose inequality (/=), where 1 == 1.0.

    %% Same integer
    equal = test_is_not_equal_asm:is_ne_test(id(42), id(42)),
    %% Different integers
    not_equal = test_is_not_equal_asm:is_ne_test(id(42), id(43)),
    %% Integer and float with same value (loose equality: 1 == 1.0)
    equal = test_is_not_equal_asm:is_ne_test(id(1), id(1.0)),
    %% Integer and float with different values
    not_equal = test_is_not_equal_asm:is_ne_test(id(1), id(2.0)),
    %% Same atom
    equal = test_is_not_equal_asm:is_ne_test(id(hello), id(hello)),
    %% Different atoms
    not_equal = test_is_not_equal_asm:is_ne_test(id(hello), id(world)),
    %% Atom and integer
    not_equal = test_is_not_equal_asm:is_ne_test(id(hello), id(42)),
    0.

id(X) -> X.
