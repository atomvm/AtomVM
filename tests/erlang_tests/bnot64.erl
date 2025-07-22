%
% This file is part of AtomVM.
%
% Copyright 2025 Davide Bettio <davide@uninstall.it>
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

-module(bnot64).
-export([start/0, mybnot/1, id/1]).

start() ->
    -16#7AFECAFF = ?MODULE:mybnot(?MODULE:id(16#7AFECAFE)),
    16#7AFECAFD = ?MODULE:mybnot(?MODULE:id(-16#7AFECAFE)),
    -16#CAFECAFF = ?MODULE:mybnot(?MODULE:id(16#CAFECAFE)),
    16#CAFECAFD = ?MODULE:mybnot(?MODULE:id(-16#CAFECAFE)),
    -16#7AFECAFE12345679 = ?MODULE:mybnot(?MODULE:id(16#7AFECAFE12345678)),
    16#7AFECAFE12345677 = ?MODULE:mybnot(?MODULE:id(-16#7AFECAFE12345678)),
    0.

mybnot(I) when is_integer(I) ->
    bnot ?MODULE:id(I);
mybnot(X) ->
    {error, X}.

id(X) ->
    X.
