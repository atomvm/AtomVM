%
% This file is part of AtomVM.
%
% Copyright 2020-2021 Davide Bettio <davide@uninstall.it>
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

-module(alisp).

-export([run/1, eval/1, booleanize/1]).

run(S) ->
    % Hack: workaround missing code load
    _ = alisp_stdlib:car([[hack]]),
    L = sexp_lexer:string(S),
    P = sexp_parser:parse(L),
    eval(P).

eval(S) when is_atom(S) ->
    case get(S) of
        undefined -> throw({unbound, S});
        Val -> Val
    end;
eval(['do', Vars, [_TestExpr, _ReturnExpr] | _Exprs] = Do) ->
    Restore = put_do_vars(Vars),
    execute_do(Do, Restore);
eval(['let', Vars | Exprs]) ->
    Restore = put_vars(Vars),
    Res = eval_list(Exprs),
    _ = delete_vars(Vars),
    _ = restore_vars(Restore),
    Res;
eval(['quote', Args]) ->
    Args;
eval(['cond' | Exps]) ->
    eval_cond_tests(Exps);
eval(['progn' | Exprs]) ->
    eval_list(Exprs);
eval(['setq', Name, Expr]) ->
    Val = eval(Expr),
    put(Name, Val),
    Val;
eval(['lambda', Args | Expr]) ->
    make_fun(Args, Expr);
eval(['defun', Name, Args | Expr]) ->
    Val = make_fun(Args, Expr),
    put({'fun', Name}, Val),
    Name;
eval([[symbol_pair, Module, Fn] | Args]) when is_atom(Module) andalso is_atom(Fn) ->
    EvaluatedArgs = eval_args(Args),
    fapply(Module, Fn, EvaluatedArgs);
eval([Fn | Args]) when is_atom(Fn) ->
    EvaluatedArgs = eval_args(Args),
    func_eval(Fn, EvaluatedArgs);
eval(NotList) when not is_list(NotList) ->
    NotList.

func_eval('funcall', [Lambda | Args]) ->
    fapply(Lambda, Args);
func_eval(Fn, EvaluatedArgs) ->
    case erlang:function_exported(alisp_stdlib, Fn, 1) of
        true ->
            alisp_stdlib:Fn(EvaluatedArgs);
        false ->
            case erlang:function_exported(erlang, Fn, length(EvaluatedArgs)) of
                true ->
                    fapply(erlang, Fn, EvaluatedArgs);
                false ->
                    case get({'fun', Fn}) of
                        undefined -> throw({undefined_fun, Fn});
                        Fun -> fapply(Fun, EvaluatedArgs)
                    end
            end
    end.

eval_args([]) ->
    [];
eval_args([A | T]) ->
    [eval(A) | eval_args(T)].

eval_cond_tests([]) ->
    [];
eval_cond_tests([[Test | Actions] | T]) ->
    case booleanize(eval(Test)) of
        true -> eval_list(Actions);
        false -> eval_cond_tests(T)
    end.

eval_list([]) ->
    [];
eval_list([Exp]) ->
    eval(Exp);
eval_list([Exp | T]) ->
    eval(Exp),
    eval_list(T).

put_vars([]) ->
    [];
put_vars([[Name, Expr] | T]) ->
    Val = eval(Expr),
    case put(Name, Val) of
        undefined -> put_vars(T);
        OldVal -> [{Name, OldVal} | put_vars(T)]
    end.

put_do_vars([]) ->
    [];
put_do_vars([[Name, Expr, _UpdateExpr] | T]) ->
    Val = eval(Expr),
    case put(Name, Val) of
        undefined -> put_do_vars(T);
        OldVal -> [{Name, OldVal} | put_vars(T)]
    end.

delete_vars([]) ->
    ok;
delete_vars([[Name, _Val] | T]) ->
    erase(Name),
    delete_vars(T).

delete_do_vars([]) ->
    ok;
delete_do_vars([[Name, _Val, _UpdateExpr] | T]) ->
    erase(Name),
    delete_do_vars(T).

restore_vars([]) ->
    ok;
restore_vars([{Name, Val} | T]) ->
    _ = put(Name, Val),
    restore_vars(T).

do_update_vars(List) ->
    Updates = execute_update_statements(List, []),
    apply_var_updates(Updates).

execute_update_statements([], Acc) ->
    Acc;
execute_update_statements([[Name, _Expr, UpdateExpr] | T], Acc) ->
    Val = eval(UpdateExpr),
    % Improper lists are ugly, but a good optimization
    execute_update_statements(T, [[Name | Val] | Acc]).

apply_var_updates([]) ->
    ok;
apply_var_updates([[Name | Value] | T]) ->
    _ = put(Name, Value),
    apply_var_updates(T).

execute_do(['do', Vars, [TestExpr, ReturnExpr] | Exprs] = Do, Restore) ->
    case booleanize(eval(TestExpr)) of
        false ->
            _ = eval_list(Exprs),
            _ = do_update_vars(Vars),
            execute_do(Do, Restore);
        true ->
            Res = eval(ReturnExpr),
            _ = delete_do_vars(Vars),
            _ = restore_vars(Restore),
            Res
    end.

make_fun(Args, Expr) ->
    ExprWithFrozen = let_frozen([progn | Expr]),
    case Args of
        [] ->
            fun() -> eval(ExprWithFrozen) end;
        [Arg1] ->
            fun(A1) -> eval(['let', [[Arg1, ['quote', A1]]], ExprWithFrozen]) end;
        [Arg1, Arg2] ->
            fun(A1, A2) ->
                eval(['let', [[Arg1, ['quote', A1]], [Arg2, ['quote', A2]]], ExprWithFrozen])
            end;
        _ ->
            throw(badarg)
    end.

let_frozen(Expr) ->
    Frozen = find_frozen_vars(Expr),
    Let = fetch_frozen(Frozen, []),
    case Let of
        [] -> Expr;
        L -> ['let', L, Expr]
    end.

fetch_frozen([], Acc) ->
    Acc;
fetch_frozen([H | T], Acc) ->
    case get(H) of
        undefined ->
            fetch_frozen(T, Acc);
        Value ->
            fetch_frozen(T, [[H, Value] | Acc])
    end.

find_frozen_vars(Expr) ->
    find_frozen_vars([Expr], []).

find_frozen_vars([], Acc) ->
    Acc;
find_frozen_vars([[_H | T1] | T2], Acc) ->
    NewAcc = find_frozen_vars(T1, Acc),
    find_frozen_vars(T2, NewAcc);
find_frozen_vars([Symbol | T], Acc) when is_atom(Symbol) ->
    find_frozen_vars(T, [Symbol | Acc]);
find_frozen_vars([_H | T], Acc) ->
    find_frozen_vars(T, Acc).

booleanize(V) ->
    case V of
        [] -> false;
        nil -> false;
        false -> false;
        true -> true
    end.

fapply(Function, Args) ->
    case Args of
        [] ->
            Function();
        [Arg1] ->
            Function(Arg1);
        [Arg1, Arg2] ->
            Function(Arg1, Arg2);
        [Arg1, Arg2, Arg3] ->
            Function(Arg1, Arg2, Arg3);
        [Arg1, Arg2, Arg3, Arg4] ->
            Function(Arg1, Arg2, Arg3, Arg4);
        [Arg1, Arg2, Arg3, Arg4, Arg5] ->
            Function(Arg1, Arg2, Arg3, Arg4, Arg5);
        _ ->
            throw(badarg)
    end.

fapply(Module, Function, Args) ->
    case Args of
        [] ->
            Module:Function();
        [Arg1] ->
            Module:Function(Arg1);
        [Arg1, Arg2] ->
            Module:Function(Arg1, Arg2);
        [Arg1, Arg2, Arg3] ->
            Module:Function(Arg1, Arg2, Arg3);
        [Arg1, Arg2, Arg3, Arg4] ->
            Module:Function(Arg1, Arg2, Arg3, Arg4);
        [Arg1, Arg2, Arg3, Arg4, Arg5] ->
            Module:Function(Arg1, Arg2, Arg3, Arg4, Arg5);
        _ ->
            throw(badarg)
    end.
