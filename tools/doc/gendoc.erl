%
% This file is part of AtomVM.
%
% Copyright 2018 Fred Dushin <fred@dushin.net>
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

-module(gendoc).

-export([main/1]).

main([LibraryName, SrcDir, TgtDir]) ->
    {ok, AllFiles} = file:list_dir(SrcDir),
    ErlFiles = [SrcDir ++ "/" ++ F || F <- AllFiles, ends_with(F, ".erl")],
    ok = edoc:run(
        ErlFiles, [
            {source_path, SrcDir},
            {dir, TgtDir},
            {application, LibraryName}
        ]
    ),
    io:format("Generated documentation for application ~p using source files ~p into ~s~n", [
        LibraryName, ErlFiles, TgtDir
    ]).

ends_with(Str, Suffix) ->
    string:find(Str, Suffix, trailing) =:= Suffix.
