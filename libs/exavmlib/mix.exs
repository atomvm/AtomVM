#
# This file is part of AtomVM.
#
# Copyright 2020 Davide Bettio <davide@uninstall.it>
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

defmodule Exavmlib.MixProject do
  use Mix.Project

  def project do
    [
      app: :exavmlib,
      version: "0.1.0",
      elixir: "~> 1.6",
      deps: deps(),
      package: package()
    ]
  end

  def application do
    []
  end

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false}
    ]
  end

  defp package() do
    [
      files: [
        "mix.exs",
        "LICENSE",
        "lib/GPIO.ex",
        "lib/Console.ex"
      ],
      description: "AtomVM Elixir library",
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/atomvm/AtomVM"}
    ]
  end
end
