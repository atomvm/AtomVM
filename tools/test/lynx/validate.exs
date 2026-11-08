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
        {:ok, {^module, [attributes: attributes]}} = :beam_lib.chunks(beam, [:attributes])

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

        {Atom.to_string(module), count + checked}
      end)

    if mode == "prepare" do
      true = laws > 0
      {:lists, lists, _path} = :code.get_object_code(:lists)
      File.write!(Path.join(work, "lists.beam"), lists)
    end

    true = :code.add_patha(String.to_charlist(work))

    expected =
      Enum.map(cases, fn entry ->
        task = Task.async(fn -> apply(String.to_atom(entry["module"]), :actual, []) end)

        value =
          case Task.yield(task, String.to_integer(timeout)) do
            {:ok, value} ->
              value

            nil ->
              Task.shutdown(task, :brutal_kill)
              raise "OTP reference timed out: #{entry["expression"]}"
          end

        if value == {:exception, :error, :undef} do
          raise "OTP reference called an undefined function: #{entry["expression"]}"
        end

        Base.encode64(:erlang.term_to_binary(value))
      end)

    File.write!(
      Path.join(work, "manifest.json"),
      JSON.encode!(%{
        fixtures: modules,
        laws: laws,
        expected: expected,
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

    failures =
      Enum.zip([cases, manifest["expected"], actuals])
      |> Enum.flat_map(fn {entry, encoded, actual} ->
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
          []
        else
          IO.puts("FAIL #{entry["expression"]}")
          IO.puts("  expected: #{inspect(expected, limit: 30)}")
          value = if actual["status"] == "ok", do: value, else: actual
          IO.puts("  actual:   #{inspect(value, limit: 30)}")
          [%{case: entry, expected: encoded, actual: actual}]
        end
      end)

    summary = %{
      laws: manifest["laws"],
      passed: length(cases) - length(failures),
      failed: length(failures)
    }

    File.write!(Path.join(work, "failures.json"), JSON.encode!(failures))
    File.write!(Path.join(work, "summary.json"), JSON.encode!(summary))

    IO.puts(
      "#{summary.laws} laws checked; #{summary.passed} matched OTP; #{summary.failed} failed"
    )
end
