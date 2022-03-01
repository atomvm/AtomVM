%
% This file is part of AtomVM.
%
% Copyright 2022 Fred Dushin <fred@dushin.net>
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

-module(test_catch).

-export([start/0]).

start() ->
    ok = test_catch(),
    0.

test_catch() ->
    ok = (catch maybe_crash(ok)),
    throwme = (catch maybe_crash(throwme)),
    {'EXIT', {{badmatch, bar}, _}} = (catch maybe_crash(crash_badmatch)),
    {'EXIT', just_because} = (catch maybe_crash(crash_exit_just_because)),
    ok.

maybe_crash(ok) ->
    ok;
maybe_crash(crash_badmatch) ->
    foo = id(bar);
maybe_crash(crash_exit_just_because) ->
    erlang:exit(just_because);
maybe_crash(throwme) ->
    throw(throwme).

id(X) -> X.
