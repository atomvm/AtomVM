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

-module(test_filename).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_join(),
    ok = test_split(),
    ok = test_join2(),
    ok = test_dirname(),
    ok = test_basename(),
    ok = test_extension(),
    ok = test_rootname(),
    ok.

test_join2() ->
    ?ASSERT_MATCH(filename:join("/usr", "local"), "/usr/local"),
    ?ASSERT_MATCH(filename:join("a", ""), "a"),
    ?ASSERT_MATCH(filename:join("", "b"), "/b"),
    ?ASSERT_MATCH(filename:join("a/", "/b"), "/b"),
    ?ASSERT_MATCH(filename:join("a/", "b/"), "a/b"),
    ok.

test_dirname() ->
    ?ASSERT_MATCH(filename:dirname("/usr/src/kalle.erl"), "/usr/src"),
    ?ASSERT_MATCH(filename:dirname("kalle.erl"), "."),
    ?ASSERT_MATCH(filename:dirname("/"), "/"),
    ?ASSERT_MATCH(filename:dirname("/usr/"), "/usr"),
    ?ASSERT_MATCH(filename:dirname("usr/src/"), "usr/src"),
    ?ASSERT_MATCH(filename:dirname("a/b/c"), "a/b"),
    ?ASSERT_MATCH(filename:dirname(""), "."),
    ?ASSERT_MATCH(filename:dirname("."), "."),
    ok.

test_basename() ->
    ?ASSERT_MATCH(filename:basename("/usr/src/kalle.erl"), "kalle.erl"),
    ?ASSERT_MATCH(filename:basename("kalle.erl"), "kalle.erl"),
    ?ASSERT_MATCH(filename:basename("/"), ""),
    ?ASSERT_MATCH(filename:basename("/usr/"), "usr"),
    ?ASSERT_MATCH(filename:basename("a/b/c"), "c"),
    ?ASSERT_MATCH(filename:basename(""), ""),
    ?ASSERT_MATCH(filename:basename("src/kalle.erl", ".erl"), "kalle"),
    ?ASSERT_MATCH(filename:basename("src/kalle.beam", ".erl"), "kalle.beam"),
    ?ASSERT_MATCH(filename:basename("kalle.erl", ".erl"), "kalle"),
    %% Only a single trailing separator marks the "directory" case
    ?ASSERT_MATCH(filename:basename("a//"), ""),
    ?ASSERT_MATCH(filename:basename("a/b//"), ""),
    ?ASSERT_MATCH(filename:basename("trailing/slash///"), ""),
    ?ASSERT_MATCH(filename:basename("a/b.erl/", ".erl"), "b.erl"),
    ?ASSERT_MATCH(filename:basename("a/", "a"), "a"),
    %% An Ext containing "/" can match across components
    ?ASSERT_MATCH(filename:basename("/a/b", "/b"), "a"),
    ?ASSERT_MATCH(filename:basename("a//b", "/b"), ""),
    ok.

test_extension() ->
    ?ASSERT_MATCH(filename:extension("foo.erl"), ".erl"),
    ?ASSERT_MATCH(filename:extension("a/b.c/foo"), ""),
    ?ASSERT_MATCH(filename:extension("foo"), ""),
    ?ASSERT_MATCH(filename:extension("a.b/c.d"), ".d"),
    ?ASSERT_MATCH(filename:extension("/x.y/z.erl"), ".erl"),
    ok.

test_rootname() ->
    ?ASSERT_MATCH(filename:rootname("foo.erl"), "foo"),
    ?ASSERT_MATCH(filename:rootname("a/b.c/foo"), "a/b.c/foo"),
    ?ASSERT_MATCH(filename:rootname("/x.y/z.erl"), "/x.y/z"),
    ?ASSERT_MATCH(filename:rootname("foo"), "foo"),
    ?ASSERT_MATCH(filename:rootname("foo.erl", ".erl"), "foo"),
    ?ASSERT_MATCH(filename:rootname("foo.beam", ".erl"), "foo.beam"),
    %% A dot extension that is the whole basename is not stripped
    ?ASSERT_MATCH(filename:rootname("a/.erl", ".erl"), "a/.erl"),
    ?ASSERT_MATCH(filename:rootname("a/.bashrc", ".bashrc"), "a/.bashrc"),
    %% A leading-dot dotfile with no directory has no root name
    ?ASSERT_MATCH(filename:rootname("."), []),
    ?ASSERT_MATCH(filename:rootname(".bashrc"), []),
    ?ASSERT_MATCH(filename:rootname(".."), "."),
    ?ASSERT_MATCH(filename:rootname(".a.b"), ".a"),
    ok.

test_join() ->
    %% Empty list raises function_clause
    ?ASSERT_ERROR(filename:join([]), function_clause),

    %% Single element: returned as-is (normalized)
    ?ASSERT_MATCH(filename:join(["foo"]), "foo"),
    ?ASSERT_MATCH(filename:join(["/"]), "/"),
    ?ASSERT_MATCH(filename:join(["/usr"]), "/usr"),

    %% Basic joining with separator
    ?ASSERT_MATCH(filename:join(["foo", "bar"]), "foo/bar"),
    ?ASSERT_MATCH(filename:join(["a", "b", "c"]), "a/b/c"),

    %% Absolute path component discards all preceding components
    ?ASSERT_MATCH(filename:join(["a", "/b"]), "/b"),
    ?ASSERT_MATCH(filename:join(["a", "b", "/c"]), "/c"),
    ?ASSERT_MATCH(filename:join(["x", "y", "/z", "w"]), "/z/w"),
    ?ASSERT_MATCH(filename:join(["/usr", "local", "bin"]), "/usr/local/bin"),

    %% Trailing separators on a component are stripped (normalized)
    ?ASSERT_MATCH(filename:join(["foo/", "bar"]), "foo/bar"),
    ?ASSERT_MATCH(filename:join(["/usr/", "local"]), "/usr/local"),
    ?ASSERT_MATCH(filename:join(["a//", "b"]), "a/b"),

    %% Redundant separators within a component are normalized
    ?ASSERT_MATCH(filename:join(["foo//bar", "baz"]), "foo/bar/baz"),
    ?ASSERT_MATCH(filename:join(["foo", "bar//baz"]), "foo/bar/baz"),

    %% Leading redundant separators on an absolute component are normalized
    ?ASSERT_MATCH(filename:join(["//foo", "bar"]), "/foo/bar"),
    ?ASSERT_MATCH(filename:join(["a", "//b"]), "/b"),

    %% Root "/" joined with a component
    ?ASSERT_MATCH(filename:join(["/", "foo"]), "/foo"),

    %% Dot and dotdot components are not resolved (passed through)
    ?ASSERT_MATCH(filename:join([".", "foo"]), "./foo"),
    ?ASSERT_MATCH(filename:join(["foo", "."]), "foo/."),
    ?ASSERT_MATCH(filename:join(["foo", ".."]), "foo/.."),

    %% A trailing separator in the final component is stripped
    ?ASSERT_MATCH(filename:join(["a/"]), "a"),
    ?ASSERT_MATCH(filename:join(["a", ""]), "a"),
    ?ASSERT_MATCH(filename:join(["", ""]), ""),
    ?ASSERT_MATCH(filename:join(["a/b///c/"]), "a/b/c"),

    %% A "." component followed by a separator is dropped ("/./" -> "/"),
    %% but a leading "./" and a trailing "/." are kept
    ?ASSERT_MATCH(filename:join("a/.", "b"), "a/b"),
    ?ASSERT_MATCH(filename:join("a", "./b"), "a/b"),
    ?ASSERT_MATCH(filename:join(["a", ".", "b"]), "a/b"),
    ?ASSERT_MATCH(filename:join(["a/./b"]), "a/b"),
    ?ASSERT_MATCH(filename:join(["a/.//./b"]), "a/b"),
    ?ASSERT_MATCH(filename:join(["a/./."]), "a/."),
    ?ASSERT_MATCH(filename:join(["a/../b"]), "a/../b"),
    ?ASSERT_MATCH(filename:join(["./a"]), "./a"),
    ?ASSERT_MATCH(filename:join(["./"]), "."),
    ?ASSERT_MATCH(filename:join(["./."]), "./."),
    ?ASSERT_MATCH(filename:join([".././"]), ".."),
    ?ASSERT_MATCH(filename:join(["/./"]), "/"),
    ?ASSERT_MATCH(filename:join(["/."]), "/."),

    ok.

test_split() ->
    %% Empty string returns empty list
    ?ASSERT_MATCH(filename:split(""), []),

    %% Root returns single-element list
    ?ASSERT_MATCH(filename:split("/"), ["/"]),

    %% Simple relative paths
    ?ASSERT_MATCH(filename:split("foo"), ["foo"]),
    ?ASSERT_MATCH(filename:split("foo/bar"), ["foo", "bar"]),
    ?ASSERT_MATCH(filename:split("a/b/c"), ["a", "b", "c"]),

    %% Absolute paths: first component is "/"
    ?ASSERT_MATCH(filename:split("/usr/local/bin"), ["/", "usr", "local", "bin"]),
    ?ASSERT_MATCH(filename:split("/foo"), ["/", "foo"]),

    %% Trailing separators are ignored
    ?ASSERT_MATCH(filename:split("foo/"), ["foo"]),
    ?ASSERT_MATCH(filename:split("/usr/local/"), ["/", "usr", "local"]),
    ?ASSERT_MATCH(filename:split("foo//"), ["foo"]),

    %% Redundant separators are treated as a single separator
    ?ASSERT_MATCH(filename:split("foo//bar"), ["foo", "bar"]),
    ?ASSERT_MATCH(filename:split("/usr//local"), ["/", "usr", "local"]),

    %% Leading redundant separators are normalized (same as single "/")
    ?ASSERT_MATCH(filename:split("//usr/local"), ["/", "usr", "local"]),

    %% Dot and dotdot components are not resolved (passed through)
    ?ASSERT_MATCH(filename:split("./foo"), [".", "foo"]),
    ?ASSERT_MATCH(filename:split("foo/./bar"), ["foo", ".", "bar"]),
    ?ASSERT_MATCH(filename:split("foo/../bar"), ["foo", "..", "bar"]),

    ok.
