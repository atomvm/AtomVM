#
# This file is part of AtomVM.
#
# Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

find_program(ERLC_PATH erlc)
find_program(ERL_PATH erl)

if (ERLC_PATH AND ERL_PATH)
    set(Erlang_FOUND TRUE)

    execute_process(COMMAND ${ERL_PATH} -eval "io:put_chars(erlang:system_info(otp_release))" -s init stop -noshell
                  OUTPUT_VARIABLE Erlang_VERSION)
    message("Found Erlang OTP ${Erlang_VERSION}")
elseif(Erlang_FIND_REQUIRED)
    message(FATAL_ERROR "Erlang or Erlang compiler not found")
endif()
