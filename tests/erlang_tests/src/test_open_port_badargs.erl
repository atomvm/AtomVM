%
% This file is part of AtomVM.
%
% Copyright 2018 Davide Bettio <davide@uninstall.it>
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

-module(test_open_port_badargs).

-export([start/0]).

start() ->
    safe_open_port({spawn, fail, "test"}, []) + safe_open_port({spawn, "echo"}, nil) * 4 +
        safe_open_port({spawn}, []) * 16.

%TODO: safe_open_port({notspawn, "echo"}, []) * 64.

safe_open_port(A, B) ->
    try open_port(A, B) of
        Any -> Any
    catch
        error:badarg -> -1;
        _:_ -> -2
    end.
