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

-module(tests).

-export([start/0]).

% Module is used when running tests with BEAM.
% When running tests with AtomVM, eunit:start/0 is used instead.
start() ->
    etest:test([
        jit_tests,
        jit_aarch64_tests,
        jit_aarch64_asm_tests,
        jit_x86_64_tests,
        jit_x86_64_asm_tests
    ]).
