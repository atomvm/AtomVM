defmodule GPIO do
  def open() do
    :erlang.open_port({:spawn, "gpio"}, [])
  end

  def set_direction(gpio, gpio_num, direction) do
    send(gpio, {self(), :set_direction, gpio_num, direction})

    receive do
      ret ->
        ret
    end
  end

  def set_level(gpio, gpio_num, level) do
    send(gpio, {self(), :set_level, gpio_num, level})

    receive do
      ret ->
        ret
    end
  end
end
