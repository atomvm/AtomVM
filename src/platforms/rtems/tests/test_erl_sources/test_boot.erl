%
% This file is part of AtomVM.
%
% Copyright 2026 Peter M. <petermm@gmail.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(test_boot).
-export([start/0]).

start() ->
    erlang:display({atomvm_rtems_boot, atomvm:platform()}),
    uart_smoke(),
    i2c_smoke(),
    gpio_smoke(),
    spi_smoke(),
    rtc_slow_smoke(),
    sleep_smoke(),
    wdt_smoke(),
    ledc_smoke(),
    adc_smoke(),
    crypto_smoke(),
    flash_smoke(),
    nvs_smoke(),
    fs_smoke(),
    system_info_smoke(),
    ok.

system_info_smoke() ->
    % Cross-platform keys (available on every AtomVM platform)
    SystemArchitecture = erlang:system_info(system_architecture),
    true = is_binary(SystemArchitecture),
    true = is_integer(erlang:system_info(wordsize)),
    true = is_integer(erlang:system_info(process_count)),
    true = is_integer(erlang:system_info(port_count)),
    true = is_integer(erlang:system_info(atom_count)),
    true = is_integer(erlang:system_info(schedulers)),
    true = is_binary(erlang:system_info(atomvm_version)),
    true = is_atom(erlang:system_info(emu_flavor)),
    erlang:display({system_info_cross_platform, SystemArchitecture, ok}),

    % Platform chip information (implementation defined per board)
    case erlang:system_info(esp32_chip_info) of
        undefined ->
            erlang:display({system_info_chip, unsupported, ok});
        ChipInfo when is_map(ChipInfo) ->
            <<"riscv32-esp-rtems">> = SystemArchitecture,
            Model = maps:get(model, ChipInfo),
            true = lists:member(Model, [esp32_c3, esp32_c5, esp32_c6, esp32_h2, esp32_s31, esp32_p4]),
            Cores = maps:get(cores, ChipInfo),
            true = is_integer(Cores) andalso Cores >= 1,
            Features = maps:get(features, ChipInfo),
            % The H2 has no Wi-Fi, so it advertises Bluetooth LE only.
            true = is_list(Features) andalso
                (lists:member(bgn, Features) orelse Model =:= esp32_p4 orelse Model =:= esp32_h2),
            erlang:display({system_info_chip, Model, Cores, Features, ok})
    end,

    erlang:display({system_info, ok}),
    ok.

uart_smoke() ->
    try uart:open([{peripheral, "/dev/console"}, {speed, 115200}]) of
        Pid when is_pid(Pid) ->
            {error, timeout} = uart:read(Pid, 20),
            erlang:display({uart, read_timeout, ok}),
            ok = uart:write(Pid, <<"uart-ok\n">>),
            erlang:display({uart, write, ok}),
            ok = uart:close(Pid)
    catch
        error:Reason ->
            erlang:display({uart, Reason, ok})
    end.

i2c_smoke() ->
    case i2c:init([{peripheral, "/dev/i2c-invalid"}, {fdt_alias, "atomvm-missing"}]) of
        {error, enotsup} ->
            {error, enotsup} = i2c:is_device_ready(make_ref(), 16#50, 1, 100),
            {error, enotsup} = i2c:transfer(make_ref(), [{write, 16#50, <<0>>}], 100),
            erlang:display({i2c, enotsup});
        {error, _Reason} ->
            erlang:display({i2c, fdt_alias, ok}),
            i2c_open_smoke();
        {ok, Resource} ->
            ok = i2c:deinit(Resource),
            erlang:error(i2c_fdt_alias_ignored)
    end.

i2c_open_smoke() ->
    case i2c:init([{peripheral, "/dev/i2c-0"}, {fdt_alias, "i2c0"}, {retries, 1}]) of
        {ok, Resource} ->
            erlang:display({i2c, open, ok}),
            ok = i2c:deinit(Resource);
        {error, Reason} ->
            erlang:error({i2c_smoke_failed, Reason})
    end.

gpio_smoke() ->
    case gpio:init(#{}) of
        {error, enotsup} ->
            {error, enotsup} = gpio:set_pin_pull(#{}, up),
            {error, enotsup} = gpio:set_pin_mode(#{}, output_od),
            {error, enotsup} = gpio:set_int_nif(#{}, rising, self()),
            {error, enotsup} = gpio:remove_int_nif(#{}),
            {error, enotsup} = gpio:set_pin_mux(#{}, 0),
            erlang:display({gpio, enotsup});
        {error, invalid_pin} ->
            {error, invalid_pin} = gpio:set_pin_pull(#{}, up),
            {error, invalid_pin} = gpio:set_pin_mode(#{}, output_od),
            {error, invalid_pin} = gpio:set_int_nif(#{}, rising, self()),
            {error, invalid_pin} = gpio:remove_int_nif(#{}),
            {error, invalid_pin} = gpio:set_pin_mux(#{}, 0),
            gpio_target_smoke(),
            erlang:display({gpio, imx7, ok});
        Other ->
            erlang:error({gpio_smoke_failed, Other})
    end.

gpio_target_smoke() ->
    case gpio:init(2) of
        ok ->
            ok = gpio:set_pin_mode(2, output),
            ok = gpio:digital_write(2, high),
            true = is_atom(gpio:digital_read(2)),
            ok = gpio:digital_write(2, low),
            true = is_atom(gpio:digital_read(2)),
            ok = gpio:set_pin_mode(2, input),
            ok = gpio:set_pin_pull(2, up),
            ok = gpio:set_pin_pull(2, down),
            ok = gpio:set_pin_pull(2, floating),
            ok = gpio:set_int_nif(2, rising, self()),
            ok = gpio:remove_int_nif(2),
            ok = gpio:attach_interrupt(2, rising),
            ok = gpio:detach_interrupt(2),
            ok = gpio:stop(),
            ok = gpio:deinit(2),
            erlang:display({gpio, esp32c3, ok});
        {error, invalid_pin} ->
            ok
    end.

spi_smoke() ->
    case spi:init([{peripheral, "/dev/spi-invalid"}, {fdt_alias, "atomvm-missing"}]) of
        {error, enotsup} ->
            erlang:display({spi, enotsup});
        {error, _Reason} ->
            erlang:display({spi, fdt_alias, ok}),
            spi_open_smoke();
        {ok, Resource} ->
            ok = spi:deinit(Resource),
            erlang:error(spi_fdt_alias_ignored)
    end.

spi_open_smoke() ->
    case spi:init([{peripheral, "/dev/spi-0"}, {fdt_alias, "spi0"}]) of
        {ok, Resource} ->
            ok = spi:apply_device_config(Resource, #{clock_speed_hz => 1000000, mode => 0}),
            erlang:display({spi, open, ok}),
            ok = spi:deinit(Resource);
        {error, _Reason} ->
            ok
    end.

rtc_slow_smoke() ->
    case catch atomvm_rtems:rtc_slow_set_binary(<<>>) of
        ok ->
            <<>> = atomvm_rtems:rtc_slow_get_binary(),
            <<>> = esp:rtc_slow_get_binary(),
            ok = atomvm_rtems:rtc_slow_set_binary(<<42>>),
            <<42>> = atomvm_rtems:rtc_slow_get_binary(),
            ok = esp:rtc_slow_set_binary(<<"atomvm_rtc_ok">>),
            <<"atomvm_rtc_ok">> = atomvm_rtems:rtc_slow_get_binary(),
            <<"atomvm_rtc_ok">> = esp:rtc_slow_get_binary(),
            LargeBin = <<0:(4087 * 8)>>,
            ok =
                try
                    atomvm_rtems:rtc_slow_set_binary(LargeBin),
                    should_fail
                catch
                    error:badarg -> ok
                end,
            <<"atomvm_rtc_ok">> = atomvm_rtems:rtc_slow_get_binary(),

            % Hardware timer & monotonic timer testing
            T1 = esp:timer_get_time(),
            true = is_integer(T1) andalso T1 >= 0,
            T2 = esp:timer_get_time(),
            true = is_integer(T2) andalso T2 >= T1,
            erlang:display({esp_timer_get_time, ok, T1, T2}),

            RtcUs = esp:rtc_get_time_us(),
            true = is_integer(RtcUs) andalso RtcUs >= 0,
            RtcCnt = atomvm_rtems:get_rtc_counter(),
            true = is_integer(RtcCnt) andalso RtcCnt >= 0,
            erlang:display({rtc_counter, ok, RtcCnt}),

            % Reset reason testing
            ResetReason = esp:reset_reason(),
            true = is_atom(ResetReason),
            erlang:display({esp_reset_reason, ResetReason}),

            % RTC Slow memory Timekeeper persistence testing
            InitStatus = atomvm_rtems:rtc_timekeeper_status(),
            erlang:display({rtc_timekeeper_initial_status, InitStatus}),

            case atomvm_rtems:rtc_timekeeper_record(1700000000, 123456) of
                {error, enodev} ->
                    erlang:display({rtc_timekeeper, unclocked});
                ok ->
            {ok, StatusMap} = atomvm_rtems:rtc_timekeeper_status(),
            #{
                sntp_sec := 1700000000,
                sntp_usec := 123456,
                rtc_counter := CntSaved,
                elapsed_sec := ElapsedSec
            } = StatusMap,
            true = is_integer(CntSaved) andalso CntSaved >= 0,
            true = is_integer(ElapsedSec) andalso ElapsedSec >= 0,
            erlang:display({rtc_timekeeper_recorded_status, ok, StatusMap}),

            true = atomvm_rtems:rtc_timekeeper_restore(),
            erlang:display({rtc_timekeeper_restored, ok})

            end,

            % Ensure user binary in RTC slow memory was preserved independently
            <<"atomvm_rtc_ok">> = atomvm_rtems:rtc_slow_get_binary(),
            <<"atomvm_rtc_ok">> = esp:rtc_slow_get_binary(),

            erlang:display({rtc_slow, esp32c3, ok});
        {'EXIT', {badarg, _}} ->
            erlang:display({rtc_slow, unsupported, ok})
    end.

crypto_smoke() ->
    case catch crypto:strong_rand_bytes(16) of
        Rand1 when is_binary(Rand1) andalso byte_size(Rand1) =:= 16 ->
            Rand2 = crypto:strong_rand_bytes(16),
            true = is_binary(Rand2) andalso byte_size(Rand2) =:= 16,
            true = (Rand1 =/= Rand2),
            erlang:display({crypto, strong_rand_bytes, ok}),

            % Hash testing
            HelloHash = crypto:hash(sha256, <<"Hello">>),
            <<24, 95, 141, 179, 34, 113, 254, 37, 245, 97, 166, 252, 147, 139, 46, 38, 67, 6, 236,
                48, 78, 218, 81, 128, 7, 209, 118, 72, 38, 56, 25, 105>> = HelloHash,
            erlang:display({crypto, sha256, ok}),

            % AES-128-ECB one-time encrypt/decrypt
            Key = <<1:128>>,
            PlainText = <<"16-byte-block-ok">>,
            CipherText = crypto:crypto_one_time(aes_128_ecb, Key, PlainText, true),
            PlainText = crypto:crypto_one_time(aes_128_ecb, Key, CipherText, false),
            erlang:display({crypto, aes_128_ecb, ok}),

            % atomvm random NIFs
            AtomRand = atomvm:rand_bytes(8),
            8 = byte_size(AtomRand),
            AtomRandInt = atomvm:random(),
            true = is_integer(AtomRandInt),
            erlang:display({crypto, atomvm_random, ok}),

            erlang:display({crypto, esp32c3, ok});
        {'EXIT', {badarg, _}} ->
            erlang:display({crypto, unsupported, ok});
        {'EXIT', {undef, _}} ->
            erlang:display({crypto, undef, ok})
    end.

sleep_smoke() ->
    % Initial wakeup cause (should be undefined on cold boot)
    WakeupCause = esp:sleep_get_wakeup_cause(),
    erlang:display({esp_sleep_get_wakeup_cause, WakeupCause}),
    true = is_atom(WakeupCause),

    % Deep sleep hold enable/disable (both gpio and esp modules)
    HoldResult = gpio:deep_sleep_hold_en(),
    true = (HoldResult =:= ok orelse HoldResult =:= {error, enotsup}),
    HoldResult = gpio:deep_sleep_hold_dis(),
    HoldResult = esp:deep_sleep_hold_en(),
    HoldResult = esp:deep_sleep_hold_dis(),
    erlang:display({deep_sleep_hold, ok}),

    % Parameter validation for sleep NIFs
    {'EXIT', {badarg, _}} = (catch esp:deep_sleep(-1)),
    {'EXIT', {badarg, _}} = (catch esp:sleep_enable_timer_wakeup(-1)),

    % Sleep wakeup source configuration
    ok = esp:sleep_enable_timer_wakeup(5000),
    ok = esp:deep_sleep_enable_gpio_wakeup(1 bsl 2, 0),
    ok = esp:sleep_enable_gpio_wakeup(),
    erlang:display({sleep_enable_wakeup, ok}),

    % Light sleep test (suspends and wakes up after timer). Wokwi's ESP32-H2
    % model stops the CPU on a sleep request and never signals a wakeup, so
    % entering sleep there would hang this test; ESP-IDF's own light- and
    % deep-sleep examples hang on H2 in Wokwi the same way. H2 sleep stays
    % covered by rtems_peripheral_test on a board.
    SleepResult =
        case erlang:system_info(esp32_chip_info) of
            #{model := esp32_h2} -> skipped;
            _ -> esp:light_sleep()
        end,
    true = lists:member(SleepResult, [skipped, ok, {error, enodev}, {error, timeout}, {error, enotsup}]),
    erlang:display({hardware_sleep_result, SleepResult}),
    WakeupAfterLight = esp:sleep_get_wakeup_cause(),
    erlang:display({esp_light_sleep, ok, WakeupAfterLight}),
    true = (WakeupAfterLight =:= sleep_wakeup_timer orelse WakeupAfterLight =:= undefined),

    erlang:display({sleep, esp32c3, ok}).

wdt_smoke() ->
    % Test invalid config badarg rejection
    {'EXIT', {badarg, _}} = (catch esp:task_wdt_reconfigure(config)),
    {'EXIT', {badarg, _}} = (catch esp:task_wdt_reconfigure({-1, 0, false})),
    {'EXIT', {badarg, _}} = (catch esp:task_wdt_reconfigure({0, 0, false})),
    {'EXIT', {badarg, _}} = (catch esp:task_wdt_reconfigure({5000, -1, false})),
    {'EXIT', {badarg, _}} = (catch esp:task_wdt_reconfigure({5000, 512, false})),
    {'EXIT', {badarg, _}} = (catch esp:task_wdt_reconfigure({5000, 0, 0})),
    erlang:display({task_wdt_badarg, ok}),

    % Valid reconfiguration
    ok = esp:task_wdt_reconfigure({5000, 0, false}),
    erlang:display({task_wdt_reconfigure, ok}),

    % Task WDT user management
    {'EXIT', {badarg, _}} = (catch esp:task_wdt_add_user(42)),
    {ok, Handle} = esp:task_wdt_add_user(<<"smoke_worker">>),
    ok = esp:task_wdt_reset_user(Handle),
    ok = esp:task_wdt_delete_user(Handle),
    erlang:display({task_wdt_user, ok}),

    % Hardware watchdog feed
    ok = esp:wdt_feed(),
    erlang:display({wdt_feed, ok}),

    % Task WDT lifecycle: deinit, reconfigure-while-down, and re-init
    ok = esp:task_wdt_deinit(),
    {error, _} = esp:task_wdt_deinit(),
    {error, noproc} = esp:task_wdt_reconfigure({5000, 0, false}),
    ok = esp:task_wdt_init({5000, 0, false}),
    {error, already_started} = esp:task_wdt_init({5000, 0, false}),
    erlang:display({task_wdt_lifecycle, ok}),

    erlang:display({wdt, esp32c3, ok}).

ledc_smoke() ->
    % Configure timer 0: 2000 Hz, 12-bit duty resolution, speed mode 1 (low speed)
    TimerConfig = [
        {duty_resolution, 12},
        {freq_hz, 2000},
        {speed_mode, 1},
        {timer_num, 0}
    ],
    ok = ledc:timer_config(TimerConfig),
    2000 = ledc:get_freq(1, 0),
    ok = ledc:set_freq(1, 0, 4000),
    4000 = ledc:get_freq(1, 0),
    ok = ledc:set_freq(1, 0, 2000),
    erlang:display({ledc_timer_config, ok}),

    % Configure channel 0: GPIO 2, channel 0, timer 0, duty 1024, hpoint 0
    ChannelConfig = [
        {channel, 0},
        {duty, 1024},
        {gpio_num, 2},
        {speed_mode, 1},
        {hpoint, 0},
        {timer_sel, 0}
    ],
    ok = ledc:channel_config(ChannelConfig),
    timer:sleep(2),
    1024 = ledc:get_duty(1, 0),
    ok = ledc:set_duty(1, 0, 2048),
    ok = ledc:update_duty(1, 0),
    timer:sleep(2),
    2048 = ledc:get_duty(1, 0),
    ok = ledc:set_duty_and_update(1, 0, 4096, 0),
    timer:sleep(2),
    4096 = ledc:get_duty(1, 0),
    erlang:display({ledc_channel_config, ok}),

    % Fade functions
    ok = ledc:fade_func_install(0),
    ok = ledc:set_fade_with_time(1, 0, 1000, 100),
    ok = ledc:fade_start(1, 0, 0),
    timer:sleep(2),
    1000 = ledc:get_duty(1, 0),
    ok = ledc:fade_stop(1, 0),
    ok = ledc:set_fade_with_step(1, 0, 500, 10, 1),
    ok = ledc:set_fade_time_and_start(1, 0, 250, 50, 0),
    timer:sleep(2),
    250 = ledc:get_duty(1, 0),
    ok = ledc:set_fade_step_and_start(1, 0, 100, 5, 1, 0),
    timer:sleep(2),
    100 = ledc:get_duty(1, 0),
    ok = ledc:fade_func_uninstall(),
    erlang:display({ledc_fade, ok}),

    % Stop channel
    ok = ledc:stop(1, 0, 0),
    erlang:display({ledc_stop, ok}),

    erlang:display({ledc, esp32c3, ok}).

adc_smoke() ->
    case esp_adc:init() of
        {error, enotsup} ->
            erlang:display({adc, unsupported, ok});
        {ok, Unit} ->
            Pin = case erlang:system_info(esp32_chip_info) of
                #{model := esp32_s31} -> 42;
                #{model := esp32_p4} -> 16;
                _ -> 2
            end,
            {'EXIT', {badarg, _}} = (catch esp_adc:acquire(59, Unit)),
            {'EXIT', {badarg, _}} = (catch esp_adc:acquire(Pin, Unit, invalid_bits, db_11)),
            {'EXIT', {badarg, _}} = (catch esp_adc:acquire(Pin, Unit, bit_12, invalid_atten)),
            {ok, Chan} = esp_adc:acquire(Pin, Unit, bit_12, db_11),
            {'EXIT', {badarg, _}} = (catch esp_adc:sample(Chan, Unit, [raw, {samples, 0}])),
            {'EXIT', {badarg, _}} = (catch esp_adc:sample(Chan, Unit, not_a_list)),
            case esp_adc:sample(Chan, Unit) of
                {ok, {Raw, Mv}} ->
                    true = is_integer(Raw) andalso Raw >= 0 andalso Raw =< 4095,
                    true = (is_integer(Mv) andalso Mv >= 0) orelse Mv =:= undefined,
                    erlang:display({esp_adc_sample_default, Raw, Mv, ok});
                {error, timeout} ->
                    erlang:display({esp_adc_sample_default, timeout})
            end,
            ok = esp_adc:release_channel(Chan),
            ok = esp_adc:deinit(Unit),
            erlang:display({adc, esp32c3, ok})
    end.

flash_smoke() ->
    case esp:flash_get_size() of
        0 ->
            erlang:display({flash, unsupported, ok});
        FlashSize when is_integer(FlashSize) andalso FlashSize >= 4194304 ->
            erlang:display({esp_flash_get_size, ok, FlashSize}),

            % Parameter validation: invalid offset / length badargs
            {'EXIT', {badarg, _}} = (catch esp:flash_read(-1, 16)),
            {'EXIT', {badarg, _}} = (catch esp:flash_read(FlashSize + 1, 16)),
            {'EXIT', {badarg, _}} = (catch esp:flash_write(-1, <<"data">>)),
            {'EXIT', {badarg, _}} = (catch esp:flash_write(0, not_a_binary)),
            {'EXIT', {badarg, _}} = (catch esp:flash_erase_sector(-1)),
            {'EXIT', {badarg, _}} = (catch esp:flash_erase_range(1, 4096)),
            erlang:display({esp_flash_badarg, ok}),

            % Low-level erase and write/read at test sector 960 (0x3C0000)
            TestOffset = 16#3C0000,
            TestSector = TestOffset div 4096,
            ok = esp:flash_erase_sector(TestSector),
            erlang:display({esp_flash_erase_sector, ok}),

            TestPattern = <<"AtomVM_Flash_LowLevel_RW_OK_1234">>,
            ok = esp:flash_write(TestOffset, TestPattern),
            {ok, ReadBack} = esp:flash_read(TestOffset, byte_size(TestPattern)),
            TestPattern = ReadBack,
            erlang:display({esp_flash_read_write, ok}),

            % atomvm_rtems alias test
            {ok, ReadBackAlias} = atomvm_rtems:flash_read(TestOffset, byte_size(TestPattern)),
            TestPattern = ReadBackAlias,

            erlang:display({flash, esp32c3, ok})
    end.

nvs_smoke() ->
    case esp:nvs_reformat() of
        error ->
            erlang:display({nvs, unsupported, ok});
        ok ->
            erlang:display({esp_nvs_reformat, ok}),

            % Fetch non-existent key
            case esp:nvs_fetch_binary(app_ns, missing_key) of
                {error, not_found} -> ok;
                {error, namespace_not_found} -> ok
            end,
            undefined = esp:nvs_get_binary(app_ns, missing_key),
            <<"my_default">> = esp:nvs_get_binary(app_ns, missing_key, <<"my_default">>),
            erlang:display({esp_nvs_not_found, ok}),

            % Put and fetch binary
            Val1 = <<"atomvm_nvs_payload_12345">>,
            ok = esp:nvs_put_binary(app_ns, key1, Val1),
            {ok, Val1} = esp:nvs_fetch_binary(app_ns, key1),
            Val1 = esp:nvs_get_binary(app_ns, key1),
            erlang:display({esp_nvs_put_fetch, ok}),

            % Update existing key
            Val2 = <<"atomvm_nvs_payload_updated_67890">>,
            ok = esp:nvs_put_binary(app_ns, key1, Val2),
            Val2 = esp:nvs_get_binary(app_ns, key1),
            erlang:display({esp_nvs_update, ok}),

            % Add second key in same namespace
            Val3 = <<"another_key_value">>,
            ok = esp:nvs_put_binary(app_ns, key2, Val3),
            Val3 = esp:nvs_get_binary(app_ns, key2),
            Val2 = esp:nvs_get_binary(app_ns, key1),
            erlang:display({esp_nvs_multiple_keys, ok}),

            % Erase single key
            ok = esp:nvs_erase_key(app_ns, key1),
            undefined = esp:nvs_get_binary(app_ns, key1),
            Val3 = esp:nvs_get_binary(app_ns, key2),
            erlang:display({esp_nvs_erase_key, ok}),

            % Erase all keys in namespace
            ok = esp:nvs_erase_all(app_ns),
            undefined = esp:nvs_get_binary(app_ns, key2),
            erlang:display({esp_nvs_erase_all, ok}),

            % Default namespace convenience functions
            ok = esp:nvs_set_binary(boot_mode, <<"fast_boot">>),
            <<"fast_boot">> = esp:nvs_get_binary(boot_mode),
            ok = esp:nvs_erase_key(boot_mode),
            undefined = esp:nvs_get_binary(boot_mode),
            erlang:display({esp_nvs_default_ns, ok}),

            erlang:display({nvs, esp32c3, ok})
    end.

fs_smoke() ->
    case esp:flash_fs_mount() of
        error ->
            erlang:display({fs, unsupported, ok});
        ok ->
            true = esp:flash_fs_is_mounted(),
            erlang:display({esp_flash_fs_mount, ok}),

            % Test file:write_file and file:read_file
            FilePath = "/flash/atomvm_test.txt",
            FileContent = <<"AtomVM persistent file storage on RTEMS ESP32-C3!\n">>,
            ok = file:write_file(FilePath, FileContent),
            {ok, ReadContent} = file:read_file(FilePath),
            FileContent = ReadContent,
            erlang:display({file_read_write, ok}),

            % Non-existent file error handling
            case file:read_file("/flash/non_existent_file.txt") of
                {error, enoent} -> ok;
                {error, _} -> ok
            end,
            erlang:display({file_not_found, ok}),

            erlang:display({fs, esp32c3, ok})
    end.
