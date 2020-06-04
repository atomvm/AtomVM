defmodule GPIO do
  def open() do
    case :erlang.whereis(:gpio) do
      :undefined ->
        :erlang.open_port({:spawn, "gpio"}, [])

      pid ->
        pid
    end
  end

  def read(gpio, gpio_num) do
    send(gpio, {self(), :read, gpio_num})

    receive do
      ret ->
        ret
    end
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

  def set_int(gpio, gpio_num, trigger) do
    send(gpio, {self(), :set_int, gpio_num, trigger})

    receive do
      ret -> ret
    end
  end

  def remove_int(gpio, gpio_num) do
    send(gpio, {self(), :remove_int, gpio_num})

    receive do
      ret -> ret
    end
  end

  def set_pin_mode(_gpio_num, _mode),
    do: throw(:nif_error)

  def digital_write(_gpio_num, _level),
    do: throw(:nif_error)

  def digital_read(_gpio_num),
    do: throw(:nif_error)

  def attach_interrupt(gpio_num, mode),
    do: set_int(open(), gpio_num, mode)

  def detach_interrupt(gpio_num),
    do: remove_int(open(), gpio_num)
end
