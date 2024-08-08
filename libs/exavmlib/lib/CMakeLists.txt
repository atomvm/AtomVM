#
# This file is part of AtomVM.
#
# Copyright 2019-2021 Riccardo Binetti <rbino@gmx.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

project(exavmlib)

include(BuildElixir)

set(ELIXIR_MODULES
    AVMPort
    Bitwise
    Code
    Console
    GPIO
    I2C
    Integer
    LEDC
    Access
    Enum
    Exception
    IO
    List
    Map
    Module
    Keyword
    Kernel
    Process
    Protocol.UndefinedError
    Tuple

    ArithmeticError
    ArgumentError
    BadFunctionError
    BadStructError
    RuntimeError
    SystemLimitError
    BadMapError
    BadBooleanError
    MatchError
    CaseClauseError
    WithClauseError
    CondClauseError
    TryClauseError
    BadArityError
    UndefinedFunctionError
    FunctionClauseError
    KeyError
    ErlangError
)

pack_archive(exavmlib ${ELIXIR_MODULES})
