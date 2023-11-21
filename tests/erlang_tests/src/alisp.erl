%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-export([run/1, start/0, eval/1, put_vars/1]).

start() ->
    run(
        "(let ((y -1))\n"
        "    (do ((i 0 (+ 1 i))) ((> i 2) y)\n"
        "        (let ((x (+ 39 1)))\n"
        "            (cond\n"
        "                ((= (+ 7 (+ (quote 5) 2)) 13) (0))\n"
        "                ((= 1 1) (setq y (+ x i)))\n"
        "                ((= 1 0) (-1))))))"
    ).

run(S) ->
    L = sexp_lexer:string(S),
    P = sexp_parser:parse(L),
    eval(P).

eval(S) when is_atom(S) ->
    case get(S) of
        undefined -> throw(unbound);
        Val -> Val
    end;
eval(I) when is_integer(I) ->
    I;
eval([do, Vars, [_TestExpr, _ReturnExpr] | _Exprs] = Do) ->
    Restore = put_do_vars(Vars),
    execute_do(Do, Restore);
eval(['let', Vars | Exprs]) ->
    Restore = put_vars(Vars),
    Res = eval_list(Exprs),
    _ = delete_vars(Vars),
    _ = restore_vars(Restore),
    Res;
eval([quote, Args]) ->
    Args;
eval(['cond' | Exps]) ->
    eval_cond_tests(Exps);
eval(['progn' | Exprs]) ->
    eval_list(Exprs);
eval(['setq', Name, Expr]) ->
    Val = eval(Expr),
    put(Name, Val),
    Val;
eval([Fn | Args]) when is_atom(Fn) ->
    EvaluatedArgs = eval_args(Args),
    func_eval(Fn, EvaluatedArgs).

func_eval('=', [Arg1, Arg2]) ->
    Arg1 == Arg2;
func_eval(Fn, EvaluatedArgs) ->
    fapply(erlang, Fn, EvaluatedArgs).

eval_args([]) ->
    [];
eval_args([A | T]) ->
    [eval(A) | eval_args(T)].

eval_cond_tests([]) ->
    [];
eval_cond_tests([[Test, Action] | T]) ->
    case eval(Test) of
        true -> eval(Action);
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

do_update_vars([]) ->
    [];
do_update_vars([[Name, _Expr, UpdateExpr] | T]) ->
    Val = eval(UpdateExpr),
    _ = put(Name, Val),
    _ = do_update_vars(T).

execute_do([do, Vars, [TestExpr, ReturnExpr] | Exprs] = Do, Restore) ->
    case eval(TestExpr) of
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
        [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6] ->
            Module:Function(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6);
        _ ->
            throw(badarg)
    end.
