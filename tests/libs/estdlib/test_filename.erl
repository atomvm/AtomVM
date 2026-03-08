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
