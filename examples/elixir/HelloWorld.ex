defmodule HelloWorld do

  def start() do
    console = do_open_port("console", [])
    write(console, "Hello World\n")
  end

  defp do_open_port(port_name, param) do
    Port.open({:spawn, port_name}, param)
  end

  defp write(console, string) do
    send console, {self(), string}

    receive do
      return_status ->
        return_status
    end
  end

end
