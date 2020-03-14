defmodule Console do
  def puts(device \\ :stdio, item) do
    pid =
      case :erlang.whereis(device) do
        :undefined ->
          case device do
            :stdio ->
              new_pid = :erlang.open_port({:spawn, "console"}, [])
              :erlang.register(:stdio, new_pid)
              new_pid

            _ ->
              :error
          end

        pid ->
          pid
      end

    write(pid, item)
  end

  defp write(console, string) do
    send(console, {self(), string})

    receive do
      return_status ->
        return_status
    end
  end
end
