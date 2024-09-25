#
# This file is part of elixir-lang.
#
# Copyright 2013-2023 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/v1.17.2/lib/elixir/lib/list/chars.ex
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
# SPDX-License-Identifier: Apache-2.0
#

defimpl List.Chars, for: Integer do
  def to_charlist(term) do
    Integer.to_charlist(term)
  end
end
