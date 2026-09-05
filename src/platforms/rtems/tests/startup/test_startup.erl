% Copyright 2026 Peter M. <petermm@gmail.com>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-module(test_startup).
-export([start/0, finish/0, unresolved/0]).

start() ->
    erlang:display(embedded_app_started),
    ?MODULE:finish().

finish() -> ok.

% Keep unresolved imports in the module even when start/0 succeeds.
unresolved() -> missing_module:missing_function().
