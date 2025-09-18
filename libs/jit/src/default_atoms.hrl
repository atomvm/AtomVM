%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

% These are hard coded and should match values in defaultatoms.def
-define(FALSE_ATOM_INDEX, 0).
-define(TRUE_ATOM_INDEX, 1).
-define(OK_ATOM_INDEX, 2).
-define(ERROR_ATOM_INDEX, 3).
-define(BADARG_ATOM_INDEX, 4).
-define(BADARITH_ATOM_INDEX, 5).
-define(BADFUN_ATOM_INDEX, 6).
-define(FUNCTION_CLAUSE_ATOM_INDEX, 7).
-define(TRY_CLAUSE_ATOM_INDEX, 8).
-define(OUT_OF_MEMORY_ATOM_INDEX, 9).
-define(BADMATCH_ATOM_INDEX, 10).
-define(CASE_CLAUSE_ATOM_INDEX, 11).
-define(IF_CLAUSE_ATOM_INDEX, 12).
-define(THROW_ATOM_INDEX, 13).
-define(UNSUPPORTED_ATOM_INDEX, 14).
-define(ALL_ATOM_INDEX, 15).
-define(LOWERCASE_EXIT_ATOM_INDEX, 16).
-define(BADRECORD_ATOM_INDEX, 17).

-define(FALSE_ATOM, ((?FALSE_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(TRUE_ATOM, ((?TRUE_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(OK_ATOM, ((?OK_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(ERROR_ATOM, ((?ERROR_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(BADARG_ATOM, ((?BADARG_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(BADARITH_ATOM, ((?BADARITH_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(BADFUN_ATOM, ((?BADFUN_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(FUNCTION_CLAUSE_ATOM,
    ((?FUNCTION_CLAUSE_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)
).
-define(TRY_CLAUSE_ATOM,
    ((?TRY_CLAUSE_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)
).
-define(OUT_OF_MEMORY_ATOM,
    ((?OUT_OF_MEMORY_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)
).
-define(BADMATCH_ATOM, ((?BADMATCH_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(CASE_CLAUSE_ATOM,
    ((?CASE_CLAUSE_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)
).
-define(IF_CLAUSE_ATOM, ((?IF_CLAUSE_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(THROW_ATOM, ((?THROW_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(UNSUPPORTED_ATOM,
    ((?UNSUPPORTED_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)
).
-define(ALL_ATOM, ((?ALL_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).
-define(LOWERCASE_EXIT_ATOM,
    ((?LOWERCASE_EXIT_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)
).
-define(BADRECORD_ATOM, ((?BADRECORD_ATOM_INDEX bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_ATOM)).

-define(DEFAULT_ATOMS, #{
    false => ?FALSE_ATOM,
    true => ?TRUE_ATOM,
    ok => ?OK_ATOM,
    error => ?ERROR_ATOM,
    badarg => ?BADARG_ATOM,
    badarith => ?BADARITH_ATOM,
    badfun => ?BADFUN_ATOM,
    function_clause => ?FUNCTION_CLAUSE_ATOM,
    try_clause => ?TRY_CLAUSE_ATOM,
    out_of_memory => ?OUT_OF_MEMORY_ATOM,
    badmatch => ?BADMATCH_ATOM,
    case_clause => ?CASE_CLAUSE_ATOM,
    if_clause => ?IF_CLAUSE_ATOM,
    throw => ?THROW_ATOM,
    unsupported => ?UNSUPPORTED_ATOM,
    all => ?ALL_ATOM,
    exit => ?LOWERCASE_EXIT_ATOM,
    badrecord => ?BADRECORD_ATOM
}).
