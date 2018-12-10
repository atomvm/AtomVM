defmodule Blink do
  def start do
    spawn(Blinker, :start, [{:d, 12}, 1000])
    spawn(Blinker, :start, [{:d, 13}, 500])
    spawn(Blinker, :start, [{:d, 14}, 1500])
    spawn(Blinker, :start, [{:d, 15}, 300])

    loop()
  end

  def loop do
    loop()
  end
end
