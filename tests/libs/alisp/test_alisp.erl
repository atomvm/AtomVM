%
% This file is part of AtomVM.
%
% Copyright 2021 Davide Bettio <davide@uninstall.it>
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

-module(test_alisp).

-export([test/0]).

-include("etest.hrl").

test() ->
    ok = test_snippet0(),
    ok.

test_snippet0() ->
    ?ASSERT_MATCH(alisp:run(snippet0()), 25),
    ?ASSERT_MATCH(alisp:run(snippet1()), 1),
    ok.

snippet0() ->
    "\n"
    "    (progn\n"
    "        (defun is-prime (n)\n"
    "            (let ((c (quote nil)) (result (quote nil)))\n"
    "                (do ((i 2 (+ i 1))) ((identity c) (identity result))\n"
    "                    (cond\n"
    "                        ((= n i)\n"
    "                            (setq result (quote true))\n"
    "                            (setq c (quote true)))\n"
    "                        ((= 0 (rem n i))\n"
    "                            (setq result (quote nil))\n"
    "                            (setq c (quote true)))\n"
    "                        ((quote true)\n"
    "                            (setq c (quote nil)))))))\n"
    "    \n"
    "    \n"
    "        (defun gen-nums (n)\n"
    "            (do ((i 0 (+ i 1)) (l (list) (cons (- n i) l))) ((= i n) (identity l))))\n"
    "            \n"
    "        (defun primes (k)\n"
    "            (remove-if-not (lambda (m) (is-prime m)) (cdr (gen-nums k))))\n"
    "            \n"
    "        (length (primes 100))\n"
    "    )\n".

snippet1() ->
    "\n"
    "      (progn\n"
    "          (setq mylambda (lambda (a b) (- a b)))\n"
    "          (funcall mylambda 1 2)\n"
    "          (funcall (lambda ()\n"
    "                       (funcall mylambda 4 3))))\n".
