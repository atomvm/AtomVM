#
# This file is part of AtomVM.
#
# Copyright 2025 AtomVM Contributors
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

defmodule WifiScan do
  @moduledoc """
  Example demonstrating WiFi network scanning on ESP32.

  This example shows how to:
  - Initialize the network driver in STA mode
  - Scan for available WiFi networks
  - Display information about discovered access points
  """

  def start() do
    IO.puts("Starting WiFi Scan Example...")

    case scan_wifi() do
      {:ok, access_points} ->
        aps =
          sort_by_rssi(access_points)
          |> filter_hidden()

        IO.puts("\n=== Scan Results ===")
        count = :erlang.length(aps)
        IO.puts("Found #{count} visible network(s)\n")

        for ap <- aps do
          print_access_point(ap)
        end

        IO.puts("\nScan completed successfully!")
        :ok

      {:error, reason} ->
        IO.puts("Scan failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp sort_by_rssi(access_points) do
    # Sort by RSSI (signal strength) descending using lists:sort/2
    :lists.sort(
      fn {_ssid1, rssi1, _ch1, _auth1, _bssid1}, {_ssid2, rssi2, _ch2, _auth2, _bssid2} ->
        rssi1 >= rssi2
      end,
      access_points
    )
  end

  defp filter_hidden(sorted),
    do: Enum.filter(sorted, &(elem(&1, 0) != ""))

  defp scan_wifi() do
    # Initialize network in STA mode (required for scanning)
    # We don't need to connect to a network, just initialize WiFi
    config = [sta: [ssid: "", psk: ""]]

    IO.puts("Initializing WiFi driver...")

    case :network.start(config) do
      {:ok, _pid} ->
        IO.puts("WiFi driver initialized. Starting scan...")
        # Give the WiFi driver a moment to initialize
        Process.sleep(1000)

        perform_scan()

      {:error, reason} ->
        IO.puts("Failed to start network driver: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp perform_scan() do
    try do
      case :network.scan() do
        {:ok, access_points} ->
          IO.puts("Scan completed. Processing results...")
          {:ok, access_points}

        {:error, reason} ->
          IO.puts("Scan error: #{inspect(reason)}")
          {:error, reason}

        other ->
          IO.puts("Unexpected scan result: #{inspect(other)}")
          {:error, :unexpected_result}
      end
    catch
      kind, error ->
        IO.puts("Exception during scan: #{inspect(kind)} - #{inspect(error)}")
        {:error, {:exception, kind, error}}
    end
  end

  defp print_access_point(ap) do
    case ap do
      {ssid, rssi, _channel, _authmode, _bssid} ->
        IO.puts(" ")
        IO.puts("  SSID: " <> format_ssid(ssid))
        IO.puts("  RSSI: " <> format_signal_strength(rssi))

      _ ->
        IO.puts("N/A")
    end
  end

  defp format_ssid(ssid) when is_binary(ssid) do
    if ssid == "" do
      "<hidden>"
    else
      # Just use the binary directly - io:format will handle it
      ssid
    end
  end

  defp format_ssid(_), do: "<invalid>"

  defp format_signal_strength(rssi) when is_integer(rssi) do
    cond do
      rssi >= -50 -> "[████] Excellent"
      rssi >= -60 -> "[███ ] Good"
      rssi >= -70 -> "[██  ] Fair"
      rssi >= -80 -> "[█   ] Weak"
      true -> "[    ] Very Weak"
    end
  end

  defp format_signal_strength(_), do: "Unknown"
end
