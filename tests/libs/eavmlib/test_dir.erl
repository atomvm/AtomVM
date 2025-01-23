%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
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

-module(test_dir).

-export([test/0]).

-include("etest.hrl").

test() ->
    case catch (atomvm:posix_opendir(".")) of
        {ok, Dir} ->
            [eof | _Entries] = all_dir_entries(Dir, []),
            ok = atomvm:posix_closedir(Dir);
        {'EXIT', _} ->
            skipped
    end.

all_dir_entries(Dir, Acc) ->
    case atomvm:posix_readdir(Dir) of
        eof ->
            [eof | Acc];
        {ok, {dirent, Inode, Name} = Dirent} when is_integer(Inode) and is_binary(Name) ->
            all_dir_entries(Dir, [Dirent | Acc])
    end.
