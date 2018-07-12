defmodule Blink do

  def start() do
    gpio = do_open_port("gpio", [])
    set_direction(gpio, 2, :output)

    loop(gpio, 0)
  end

  defp loop(gpio, 0) do
    set_level(gpio, 2, 0)
    sleep(200)

    loop(gpio, 1)
  end

  defp loop(gpio, 1) do
    set_level(gpio, 2, 1)
    sleep(200)

    loop(gpio, 0)
  end

  defp do_open_port(port_name, param) do
    Port.open({:spawn, port_name}, param)
  end

  defp sleep(t) do
    receive do
    after t -> :ok
    end
  end

  defp set_direction(gpio, gpio_num, direction) do
    send gpio, {self(), :set_direction, gpio_num, direction}

    receive do
      ret ->
        ret
    end
  end

  defp set_level(gpio, gpio_num, level) do
    send gpio, {self(), :set_level, gpio_num, level}

    receive do
      ret ->
        ret
    end
  end

end
