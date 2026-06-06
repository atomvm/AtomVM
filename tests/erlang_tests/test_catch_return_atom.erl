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

-module(test_catch_return_atom).

-export([start/0]).

start() ->
    %% Normal returns of class-named atoms must come back unchanged.
    error = (catch return_atom(error)),
    exit = (catch return_atom(exit)),
    throw = (catch return_atom(throw)),
    %% Sanity: other atoms and shapes must also come back unchanged.
    badarith = (catch return_atom(badarith)),
    badarg = (catch return_atom(badarg)),
    ok = (catch return_atom(ok)),
    {error, x} = (catch return_term({error, x})),
    [error] = (catch return_term([error])),
    %% Real exceptions must still convert correctly.
    {'EXIT', {real_error, _Stk}} = (catch do_error()),
    {'EXIT', real_exit} = (catch do_exit()),
    real_throw = (catch do_throw()),
    %% Same checks inside a nested catch, to exercise state reset.
    error = (catch (catch return_atom(error))),
    {'EXIT', {real_error, _Stk2}} = (catch (catch do_error())),
    0.

return_atom(A) -> A.
return_term(T) -> T.

do_error() -> erlang:error(real_error).
do_exit() -> erlang:exit(real_exit).
do_throw() -> erlang:throw(real_throw).
