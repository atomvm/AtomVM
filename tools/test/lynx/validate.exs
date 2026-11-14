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

    # The protocol fixture calls consolidated support modules compiled by Lynx.
    # Copy their executable beams, not model translations or OTP library modules.
    support =
      if Enum.any?(modules, &(&1.name == "protocols")) do
        Path.wildcard("_build/#{Mix.env()}/lib/lynx/ebin/Elixir.Lynx.Support.*.beam")
        |> Enum.map(fn path ->
          module = path |> Path.basename(".beam") |> String.to_atom()
          {^module, beam, _file} = :code.get_object_code(module)
          name = Atom.to_string(module)
          File.write!(Path.join(work, "#{name}.beam"), beam)
          name
        end)
      else
        []
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
        support: support,
        exports: Enum.flat_map(modules, & &1.exports),
        laws: laws,
        expected: Enum.map(references, & &1.value),
        oracle_errors: Enum.map(references, & &1.error),
        otp: :erlang.system_info(:otp_release) |> List.to_string(),
        otp_word_size: :erlang.system_info(:wordsize)
      })
    )

  ["compare", work] ->
    read = fn name -> work |> Path.join(name) |> File.read!() |> JSON.decode!() end
    cases = read.("cases.json")
    manifest = read.("manifest.json")
    actuals = read.("actuals.json")
    run_path = Path.join(work, "run.json")
    run = if File.exists?(run_path), do: read.("run.json"), else: %{}
    target = Map.get(run, "target", %{})
    otp_word_size = Map.get(manifest, "otp_word_size", :erlang.system_info(:wordsize))
    target_word_size = Map.get(target, "word_size", otp_word_size)
    target_float_bits = Map.get(target, "float_bits", 64)
    true = length(cases) == length(actuals) and length(cases) == length(manifest["expected"])
    decode = fn value -> value |> Base.decode64!() |> :erlang.binary_to_term() end

    contains_float? = fn
      val, _self when is_float(val) ->
        true

      val, self when is_tuple(val) ->
        val |> Tuple.to_list() |> Enum.any?(&self.(&1, self))

      [h | t], self ->
        self.(h, self) or self.(t, self)

      val, self when is_map(val) ->
        Enum.any?(val, fn {k, v} -> self.(k, self) or self.(v, self) end)

      _, _ ->
        false
    end

    results =
      Enum.zip([cases, manifest["expected"], actuals, manifest["oracle_errors"]])
      |> Enum.map(fn
        {entry, nil, actual, error} ->
          timeout = error == "OTP reference timed out"
          IO.puts("UNVERIFIED #{entry["expression"]}: #{error}")

          %{
            status: :unverified,
            stage: "oracle",
            timeout: timeout,
            case: entry,
            message: error,
            actual: actual
          }

        {entry, encoded, actual, nil} ->
          expected = decode.(encoded)

          cond do
            entry["word_size_dependent"] == true and target_word_size != otp_word_size ->
              message =
                "Target word size (#{target_word_size} bytes) differs from OTP oracle (#{otp_word_size} bytes)"

              IO.puts("UNVERIFIED #{entry["expression"]}: #{message}")

              %{
                status: :unverified,
                stage: "oracle",
                timeout: false,
                case: entry,
                message: message,
                actual: actual
              }

            target_float_bits == 32 and not is_binary(entry["float32_expected"]) and
                (entry["float32_unverified"] == true or
                   contains_float?.(expected, contains_float?)) ->
              message = "Target uses float32; host binary64 OTP expectation is not applicable"
              IO.puts("UNVERIFIED #{entry["expression"]}: #{message}")

              %{
                status: :unverified,
                stage: "oracle",
                timeout: false,
                case: entry,
                message: message,
                actual: actual
              }

            true ->
              {expected_encoded, expected} =
                if target_float_bits == 32 and is_binary(entry["float32_expected"]) do
                  {entry["float32_expected"], decode.(entry["float32_expected"])}
                else
                  {encoded, expected}
                end

              {actual, value} =
                if actual["status"] == "ok" do
                  try do
                    {actual, decode.(actual["value"])}
                  rescue
                    ArgumentError ->
                      {%{"status" => "error", "stage" => "transport", "timeout" => false, "message" => "Invalid serialized result"}, nil}
                  end
                else
                  {actual, nil}
                end

              assertion_ok =
                entry["assert_true"] != true or (expected === {:ok, true} and value === {:ok, true})

              if actual["status"] == "ok" and value === expected and assertion_ok do
                unless Map.has_key?(entry, "operation"), do: IO.puts("PASS #{entry["expression"]}")
                %{status: :passed, case: entry}
              else
                stage = if actual["status"] == "ok", do: "semantic", else: Map.get(actual, "stage", "semantic")
                timeout = if actual["status"] == "ok", do: false, else: Map.get(actual, "timeout", false)
                IO.puts("FAIL #{entry["expression"]}")
                unless assertion_ok, do: IO.puts("  assertion: OTP and AtomVM must return true")
                IO.puts("  expected: #{inspect(expected, limit: 30)}")
                value = if actual["status"] == "ok", do: value, else: actual
                IO.puts("  actual:   #{inspect(value, limit: 30)}")

                %{
                  status: :failed,
                  stage: stage,
                  timeout: timeout,
                  case: entry,
                  expected: expected_encoded,
                  actual: actual
                }
              end
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
