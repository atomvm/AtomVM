# Copyright 2026 Peter Madsen-Mygdal <petermm@gmail.com>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

# Invoked by validate.py, optionally through a Lynx Mix project.
case System.argv() do
  [mode, work, timeout] when mode in ["prepare", "prepare_generated"] ->
    cases = work |> Path.join("cases.json") |> File.read!() |> JSON.decode!()

    fixtures =
      if mode == "prepare", do: Path.wildcard("test/fixtures/translations/*.erl"), else: []

    {modules, laws} =
      Enum.map_reduce(fixtures, 0, fn source, count ->
        {:ok, module, beam, _warnings} =
          :compile.file(String.to_charlist(source), [
            :binary,
            :debug_info,
            :return_errors,
            :return_warnings
          ])

        File.write!(Path.join(work, "#{module}.beam"), beam)
        {:module, ^module} = :code.load_binary(module, String.to_charlist(source), beam)

        {:ok, {^module, [attributes: attributes, exports: exports]}} =
          :beam_lib.chunks(beam, [:attributes, :exports])

        checked =
          if Keyword.has_key?(attributes, :law) do
            reports = Lynx.Laws.verify([beam], cache: false)
            IO.inspect(reports, label: "Proofs for #{module}", limit: :infinity)
            true = Enum.all?(reports, &(&1.status == :ok and not &1.cached))
            checked = Enum.sum(Enum.map(reports, &length(&1.theorems)))
            true = checked > 0
            checked
          else
            0
          end

        exports =
          for {name, arity} <- exports,
              name != :module_info,
              do: [Atom.to_string(module), Atom.to_string(name), arity]

        {%{name: Atom.to_string(module), exports: exports}, count + checked}
      end)

    if mode == "prepare" do
      true = laws > 0
    end

    true = :code.add_patha(String.to_charlist(work))

    references =
      Enum.map(cases, fn entry ->
        task = Task.async(fn -> apply(String.to_atom(entry["module"]), :actual, []) end)

        case Task.yield(task, String.to_integer(timeout)) do
          {:ok, {:exception, :error, :undef}} ->
            %{value: nil, error: "OTP reference called an undefined function"}

          {:ok, value} ->
            %{value: Base.encode64(:erlang.term_to_binary(value)), error: nil}

          nil ->
            Task.shutdown(task, :brutal_kill)
            IO.puts("UNVERIFIED OTP reference timed out: #{entry["expression"]}")
            %{value: nil, error: "OTP reference timed out"}
        end
      end)

    File.write!(
      Path.join(work, "manifest.json"),
      JSON.encode!(%{
        fixtures: Enum.map(modules, & &1.name),
        exports: Enum.flat_map(modules, & &1.exports),
        laws: laws,
        expected: Enum.map(references, & &1.value),
        oracle_errors: Enum.map(references, & &1.error),
        otp: :erlang.system_info(:otp_release) |> List.to_string()
      })
    )

  ["compare", work] ->
    read = fn name -> work |> Path.join(name) |> File.read!() |> JSON.decode!() end
    cases = read.("cases.json")
    manifest = read.("manifest.json")
    actuals = read.("actuals.json")
    true = length(cases) == length(actuals) and length(cases) == length(manifest["expected"])
    decode = fn value -> value |> Base.decode64!() |> :erlang.binary_to_term() end

    results =
      Enum.zip([cases, manifest["expected"], actuals, manifest["oracle_errors"]])
      |> Enum.map(fn
        {entry, nil, actual, error} ->
          IO.puts("UNVERIFIED #{entry["expression"]}: #{error}")
          %{status: :unverified, case: entry, message: error, actual: actual}

        {entry, encoded, actual, nil} ->
          expected = decode.(encoded)

          {actual, value} =
            if actual["status"] == "ok" do
              try do
                {actual, decode.(actual["value"])}
              rescue
                ArgumentError ->
                  {%{"status" => "error", "message" => "Invalid serialized result"}, nil}
              end
            else
              {actual, nil}
            end

          if actual["status"] == "ok" and value === expected do
            unless Map.has_key?(entry, "operation"), do: IO.puts("PASS #{entry["expression"]}")
            %{status: :passed, case: entry}
          else
            IO.puts("FAIL #{entry["expression"]}")
            IO.puts("  expected: #{inspect(expected, limit: 30)}")
            value = if actual["status"] == "ok", do: value, else: actual
            IO.puts("  actual:   #{inspect(value, limit: 30)}")
            %{status: :failed, case: entry, expected: encoded, actual: actual}
          end
      end)

    # This is direct-call coverage, not instrumentation or branch coverage.
    coverage =
      Enum.map(manifest["exports"], fn export ->
        calls = Enum.filter(results, &(&1.case["fixture"] == export))

        %{
          export: export,
          cases: length(calls),
          passed: Enum.count(calls, &(&1.status == :passed)),
          failed: Enum.count(calls, &(&1.status == :failed)),
          unverified: Enum.count(calls, &(&1.status == :unverified))
        }
      end)

    uncovered = Enum.filter(coverage, &(&1.cases == 0))
    for entry <- uncovered, do: IO.puts("UNCOVERED #{inspect(entry.export)}")
    failures = Enum.filter(results, &(&1.status == :failed))
    unverified = Enum.filter(results, &(&1.status == :unverified))

    summary = %{
      laws: manifest["laws"],
      passed: Enum.count(results, &(&1.status == :passed)),
      failed: length(failures),
      unverified: length(unverified),
      uncovered: length(uncovered)
    }

    File.write!(Path.join(work, "failures.json"), JSON.encode!(failures))
    File.write!(Path.join(work, "unverified.json"), JSON.encode!(unverified))
    File.write!(Path.join(work, "coverage.json"), JSON.encode!(coverage))
    File.write!(Path.join(work, "summary.json"), JSON.encode!(summary))

    IO.puts(
      "#{summary.laws} laws checked; #{summary.passed} matched OTP; #{summary.failed} failed; " <>
        "#{summary.unverified} unverified; #{summary.uncovered} uncovered exports"
    )
end
