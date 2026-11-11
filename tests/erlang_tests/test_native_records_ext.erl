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

-module(test_native_records_ext).
-export([make_vector/3, make_unexported/2]).

-export_record([vector, tagged]).
-record(#vector{x = 0, y = 0, z = 0}).

-record #unexported{a, b}.

-record(#tagged{
    tag = ext_default_tag,
    items = [ext_one, ext_two, ext_three],
    note = ext_default_note
}).

make_vector(X, Y, Z) ->
    #vector{x = X, y = Y, z = Z}.

make_unexported(A, B) ->
    #unexported{a = A, b = B}.
