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

-module(test_port).

-export([test/0]).

test() ->
    ConsolePort = open_port({spawn, "console"}, []),

    ok = port:call(ConsolePort, flush),
    ok = port:call(ConsolePort, flush, 1000),
    {error, badarg} = port:call(ConsolePort, unknown_cmd),
    ConsolePort ! {self(), close},
    receive
        {ConsolePort, closed} -> ok
    end,
    {error, noproc} = port:call(ConsolePort, flush),
    ok.
