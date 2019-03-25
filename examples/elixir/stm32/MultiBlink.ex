defmodule MultiBlink do
  def start do
    spawn(__MODULE__, :blink, [{:d, 12}, 1000])
    spawn(__MODULE__, :blink, [{:d, 13}, 500])
    spawn(__MODULE__, :blink, [{:d, 14}, 1500])
    spawn(__MODULE__, :blink, [{:d, 15}, 300])

    loop()
  end

  def loop do
    loop()
  end

  def blink(gpio, interval_ms) do
    gpio_driver = GPIO.open()
    GPIO.set_direction(gpio_driver, gpio, :output)

    blink_loop(gpio_driver, gpio, interval_ms, 0)
  end

  def blink_loop(gpio_driver, gpio, interval_ms, level) do
    GPIO.set_level(gpio_driver, gpio, level)

    :avm_timer.sleep(interval_ms)

    blink_loop(gpio_driver, gpio, interval_ms, 1 - level)
  end
end
