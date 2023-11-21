%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(count_char2_bs).

-export([start/0, count/3]).

start() ->
    count(<<"oHelloo">>, $l, 0).

count(<<A, Rest/binary>>, A, N) ->
    count(Rest, A, N + 1);
count(<<_Byte, Rest/binary>>, A, N) ->
    case count(Rest, A, N) of
        2 -> count(Rest, $o, 0) * 1000 + N;
        3 -> count(Rest, $z, 0);
        4 -> count(Rest, $o, 100);
        5 -> count(Rest, $q, -100);
        M -> M
    end;
count(<<>>, _A, N) ->
    N.
