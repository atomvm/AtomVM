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

-module(catch_from_other_module).

-export([start/0]).

start() ->
    A =
        try raise_badmatch:do_raise(-1, 3) of
            _Res -> -10
        catch
            error:{badmatch, 6} -> 1;
            _:_ -> 1024
        end,
    B =
        try raise_case_end:do_raise(1) of
            _Res2 -> -20
        catch
            error:{case_clause, 1} -> 2;
            _:_ -> 2048
        end,
    C =
        try raise_if_end:do_raise(0, 0) of
            _Res3 -> -40
        catch
            error:if_clause -> 4;
            _:_ -> 4096
        end,
    A + B + C.
