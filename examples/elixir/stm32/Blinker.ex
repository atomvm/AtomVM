defmodule Blinker do
  def start(gpio, interval_ms) do
    gpio_driver = GPIO.open();
    GPIO.set_direction(gpio_driver, gpio, :output)

    loop(gpio_driver, gpio, interval_ms, 0)
  end

  def loop(gpio_driver, gpio, interval_ms, level) do
    GPIO.set_level(gpio_driver, gpio, level)

    :timer.sleep(interval_ms)

    loop(gpio_driver, gpio, interval_ms, 1 - level)
  end
end
