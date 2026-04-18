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

% Tests runtime JIT compilation via jit_stream_flash.
% test_jit_simple is pre-loaded as plain BEAM (no native code) by the C test.
% Calling code_server:load/1 triggers the JIT compiler to compile it at runtime
% and store the result in flash, then execute it natively.
-module(test_jit_compile).

-export([start/0]).

start() ->
    ok = code_server:load(test_jit_simple),
    42 = test_jit_simple:run(),
    ok.
