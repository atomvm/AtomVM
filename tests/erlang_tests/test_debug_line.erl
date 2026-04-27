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

-module(test_debug_line).
-if(?OTP_RELEASE >= 28).
-compile([beam_debug_info]).
-endif.
-export([start/0, test_debug_line/1]).

start() ->
    ok = test_debug_line(42),
    0.

% lldb -b \
%   -o "settings set plugin.jit-loader.gdb.enable on" \
%   -o "breakpoint set -n 'test_debug_line:start/0'" \
%   -o "breakpoint set -f test_debug_line.erl -l 49" \
%   -o "breakpoint set -f test_debug_line.erl -l 52" \
%   -o "run" \
%   -o "c" \
%   -o "print term_to_int(N)" \
%   -o "c" \
%   -o "print term_to_int(Z)" \
%   -- tests/test-erlang test_debug_line
%
% Will print:
% (avm_int_t) 42
% and
% (avm_int_t) 2

test_debug_line(N) ->
    M = N + 1,
    X = M * 2,
    Z = X div N,
    2 = Z,
    ok.
