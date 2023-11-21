%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(test_list_match).

-export([start/0]).

start() ->
    Res1 =
        case f(foo) of
            foo -> 1;
            _ -> 0
        end,
    Res2 =
        case f(1342) of
            1342 -> 2;
            _ -> 0
        end,
    Res3 =
        case f({foo, bar}) of
            {foo, bar} -> 4;
            _ -> 0
        end,
    Res4 =
        case f([]) of
            [] -> 8;
            _ -> 0
        end,
    Res5 =
        case f([foo, bar]) of
            [foo, bar] -> 16;
            _ -> 0
        end,
    Res1 + Res2 + Res3 + Res4 + Res5.

f(X) when is_atom(X) -> id(X);
f(X) when is_integer(X) -> id(X);
f(X) when is_tuple(X) -> id(X);
f(X) when is_list(X) -> id(X).

id(X) -> X.
