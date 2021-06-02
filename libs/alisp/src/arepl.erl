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

-module(arepl).

-export([start/0]).

start() ->
    % HACK: workaround missing code load
    alisp_stdlib:car([[hack]]),
    io:put_chars("AtomVM LISP REPL\n\n"),
    loop([], 0, 0).

loop(PrevTokens, Count, SuccessCount) ->
    Line = io:get_line(prompt(Count, SuccessCount)),
    case sexp_lexer:string(Line) of
        [] ->
            loop(PrevTokens, Count, SuccessCount);
        Tokens ->
            NewCount = check_balance(Tokens, Count),
            NewTokens = PrevTokens ++ Tokens,
            if
                NewCount == 0 ->
                    Parsed = sexp_parser:parse(NewTokens),
                    NewSuccessCount = do_eval(Parsed, SuccessCount),
                    loop([], 0, NewSuccessCount);
                true ->
                    loop(NewTokens, NewCount, SuccessCount)
            end
    end.

do_eval(Parsed, SuccessCount) ->
    try alisp:eval(Parsed) of
        Result ->
            try println(Result) of
                _Any ->
                    SuccessCount + 1
            catch
                C:E ->
                    println({'catch-failed-print', C, E}),
                    SuccessCount
            end
    catch
        C:E ->
            println({'catch', C, E}),
            SuccessCount
    end.

prompt(PCount, SuccessCount) ->
    case PCount of
        0 -> "arepl(" ++ integer_to_list(SuccessCount) ++ ")> ";
        NotZero -> dots(NotZero, []) ++ "(" ++ integer_to_list(SuccessCount) ++ ")> "
    end.

dots(0, Acc) ->
    Acc;
dots(N, Acc) ->
    dots(N - 1, [$. | Acc]).

check_balance([], Count) ->
    Count;
check_balance([{'(', _Line} | T], Count) ->
    check_balance(T, Count + 1);
check_balance([{')', _Line} | T], Count) ->
    check_balance(T, Count - 1);
check_balance([_Any | T], Count) ->
    check_balance(T, Count).

println(Value) ->
    IOList = sexp_serializer:serialize(Value),
    Bin = erlang:iolist_to_binary([IOList, $\n]),
    io:put_chars(Bin).
