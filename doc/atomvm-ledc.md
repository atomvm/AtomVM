# LED Control API

The LED Control API can be used to drive LEDs, as well as generate PWM signals on GPIO pins.

The LED Control API is implemented using the IDF SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC API</a>.

Two APIs are provided:

* A low-level API, which is a direct mapping onto the IDF SDK
* A high-level API, which provides a simplified version of the low-level API and which is less error-prone.

> Note. The high-level API is under development.

## Low-level API

The low-level API is a direct translation of the IDF SDK <a href="https://docs.espressif.com/projects/esp-idf/en/latest/esp32/api-reference/peripherals/ledc.html">LEDC API</a>, with a natural mapping into Erlang.  This API is intended for users with complex use-cases, and who require low-level access to the LEDC APIs.

The `ledc.hrl` module should be used for common modes, channels, duty cycle resolutions, and so forth.

    -include("ledc.hrl").

    ...

    %% create a 5khz timer
    SpeedMode = ?LEDC_HIGH_SPEED_MODE,
    Channel = ?LEDC_CHANNEL_0,
    ledc:timer_config([
        {duty_resolution, ?LEDC_TIMER_13_BIT},
        {freq_hz, 5000},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {timer_num, ?LEDC_TIMER_0}
    ]).

    %% bind pin 2 to this timer in a channel
    ledc:channel_config([
        {channel, Channel},
        {duty, 0},
        {gpio_num, 2},
        {speed_mode, ?LEDC_HIGH_SPEED_MODE},
        {hpoint, 0},
        {timer_sel, ?LEDC_TIMER_0}
    ]).

    %% set the duty cycle to 0, and fade up to 16000 over 5 seconds
    ledc:set_duty(SpeedMode, Channel, 0).
    ledc:update_duty(SpeedMode, Channel).
    TargetDuty = 16000.
    FadeMs = 5000.
    ok = ledc:set_fade_with_time(SpeedMode, Channel, TargetDuty, FadeMs).


## High-level API

> The high-level APIs provide some abstractions and state management around the low-level APIs, and helps to mitigate common errors with use of the low-level APIs.

    %% create a 5khz timer
    Freq = 5000.
    {ok, Timer} = ledc_pwm:create_timer(Freq).

    %% bind pin 2 to this timer in a channel
    Pin = 2.
    {ok, Channel} = ledc_pwm:create_channel(Timer, Pin).

    io:format("Frequency: ~p Duty%: ~p~n", [ledc_pwm:get_freq_hz(Channel), ledc_pwm:get_dutyp(Channel)]).

    %% set the duty cycle to 0%, and fade up to 100% over 5 seconds
    TargetDutyp = 100.
    FadeMs = 5000.
    ledc_pwm:set_dutyp(Channel, 0).
    ledc_pwm:fade(Channel, TargetDutyp, FadeMs).
