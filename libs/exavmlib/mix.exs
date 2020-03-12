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
    [
    ]
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
      links: %{"GitHub" => "https://github.com/bettio/AtomVM"}
    ]
  end
end
