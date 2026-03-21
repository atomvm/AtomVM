%
% This file is part of AtomVM.
%
% Copyright 2025-2026 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Parse transform that instruments jit:first_pass/4 clauses with
%% DWARF opcode tracking calls. For each clause that matches a binary
%% pattern <<OpcodeInt, .../binary>>, it injects
%%   MSt0 = MMod:dwarf_opcode(MSt0__pt, OpcodeInt)
%% at the start of the function body, renaming the original MSt0 parameter.
%%
%% Clauses for OP_LABEL (opcode 1), OP_LINE (opcode 153), and OP_FUNC_INFO
%% (opcode 2) are skipped as they use dedicated macros (DWARF_LABEL,
%% DWARF_LINE, DWARF_FUNCTION) in jit.erl.
-module(jit_dwarf_pt).

-export([parse_transform/2]).

-define(OP_LABEL, 1).
-define(OP_FUNC_INFO, 2).
-define(OP_LINE, 153).

parse_transform(Forms, _Options) ->
    [transform_form(Form) || Form <- Forms].

transform_form({function, Line, first_pass, 4, Clauses}) ->
    {function, Line, first_pass, 4, [transform_clause(C) || C <- Clauses]};
transform_form(Other) ->
    Other.

transform_clause({clause, Line, [Arg1, Arg2, Arg3, Arg4], Guards, Body} = Clause) ->
    case extract_opcode(Arg1) of
        {ok, Opcode} when Opcode =/= ?OP_LABEL, Opcode =/= ?OP_FUNC_INFO, Opcode =/= ?OP_LINE ->
            case Arg3 of
                {var, VarLine, VarName} ->
                    PtName = list_to_atom(atom_to_list(VarName) ++ "__pt"),
                    NewArg3 = {var, VarLine, PtName},
                    PtVar = {var, Line, PtName},
                    OrigVar = {var, Line, VarName},
                    %% MSt0 = MMod:dwarf_opcode(MSt0__pt, OpcodeInt)
                    DwarfCall =
                        {match, Line, OrigVar,
                            {call, Line, {remote, Line, Arg2, {atom, Line, dwarf_opcode}}, [
                                PtVar, {integer, Line, Opcode}
                            ]}},
                    {clause, Line, [Arg1, Arg2, NewArg3, Arg4], Guards, [DwarfCall | Body]};
                _ ->
                    io:format(
                        standard_error,
                        "jit_dwarf_pt: warning: first_pass/4 clause at line ~p has non-variable "
                        "3rd arg, DWARF opcode tracking skipped~n",
                        [Line]
                    ),
                    Clause
            end;
        _ ->
            Clause
    end;
transform_clause(Other) ->
    Other.

extract_opcode({bin, _, [{bin_element, _, {integer, _, Opcode}, _, _} | _]}) ->
    {ok, Opcode};
extract_opcode(_) ->
    false.
