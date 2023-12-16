%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(sx127x).
-export([start/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-define(REG_FIFO, 16#00).
-define(REG_OP_MODE, 16#01).
-define(REG_FRF_MSB, 16#06).
-define(REG_FRF_MID, 16#07).
-define(REG_FRF_LSB, 16#08).
-define(REG_PA_CONFIG, 16#09).
-define(REG_LR_OCP, 16#0B).
-define(REG_LNA, 16#0C).
-define(REG_FIFO_ADDR_PTR, 16#0D).
-define(REG_FIFO_TX_BASE_ADDR, 16#0E).
-define(REG_FIFO_RX_BASE_ADDR, 16#0F).
-define(REG_FIFO_RX_CURRENT_ADDR, 16#10).
-define(REG_IRQ_FLAGS, 16#12).
-define(REG_RX_NB_BYTES, 16#13).
-define(REG_PKT_RSSI_VALUE, 16#1A).
-define(REG_PKT_SNR_VALUE, 16#1B).
-define(REG_MODEM_CONFIG_1, 16#1D).
-define(REG_MODEM_CONFIG_2, 16#1E).
-define(REG_PREAMBLE_MSB, 16#20).
-define(REG_PREAMBLE_LSB, 16#21).
-define(REG_PAYLOAD_LENGTH, 16#22).
-define(REG_MODEM_CONFIG_3, 16#26).
-define(REG_RSSI_WIDEBAND, 16#2C).
-define(REG_DETECTION_OPTIMIZE, 16#31).
-define(REG_DETECTION_THRESHOLD, 16#37).
-define(REG_SYNC_WORD, 16#39).
-define(REG_DIO_MAPPING_1, 16#40).
-define(REG_VERSION, 16#42).
-define(REG_PADAC, 16#4D).

-define(MODE_LONG_RANGE_MODE, 16#80).
-define(MODE_SLEEP, 16#00).
-define(MODE_STDBY, 16#01).
-define(MODE_TX, 16#03).
-define(MODE_RX_CONTINUOUS, 16#05).
-define(MODE_RX_SINGLE, 16#06).

-define(AUTO_AGC_FLAG, 16#04).

-define(IRQ_TX_DONE_MASK, 16#08).
-define(IRQ_PAYLOAD_CRC_ERROR_MASK, 16#20).
-define(IRQ_RX_DONE_MASK, 16#40).

-define(SPISettings, [
    {bus_config, [
        {miso, 19},
        {mosi, 27},
        {sclk, 5}
    ]},
    {device_config, [
        {my_device, [
            {clock_speed_hz, 1000000},
            {mode, 0},
            {cs, 18},
            {address_len_bits, 8}
        ]}
    ]}
]).

start() ->
    {ok, P} = gen_server:start(?MODULE, [], []),
    gen_server:call(P, init),
    loop(P).

loop(P) ->
    timer:sleep(1000),
    TheList = "AVM",
    gen_server:call(P, {send, TheList}),
    io:format("Sending: ~s~n", [TheList]),
    loop(P).

init(_) ->
    {ok, {}}.

handle_call(init, _From, _State) ->
    GPIO = gpio:open(),
    {ok, SPI} = init_sx127x(GPIO, ?SPISettings),
    {reply, ok, SPI};
handle_call({send, Msg}, _From, SPI) ->
    broadcast_packet(SPI, Msg),
    enable_receive(SPI),
    {reply, ok, SPI};
handle_call(Call, _From, State) ->
    erlang:display(Call),
    {reply, ok, State}.

handle_info({gpio_interrupt, 26}, SPI) ->
    handle_irq(SPI),
    {noreply, SPI}.

terminate(_Reason, _State) ->
    ok.

init_sx127x(GPIO, SPISettings) ->
    case init_hw(GPIO, SPISettings) of
        {ok, SPI} ->
            init_regs(SPI),
            write_register(SPI, ?REG_DIO_MAPPING_1, 16#00),
            gpio:set_int(GPIO, 26, rising),
            enable_receive(SPI),
            {ok, SPI};
        Error ->
            erlang:display(Error),
            Error
    end.

init_hw(GPIO, SPISettings) ->
    gpio:set_direction(GPIO, 26, input),
    gpio:set_direction(GPIO, 14, output),

    % Reset
    gpio:set_level(GPIO, 14, 0),
    timer:sleep(20),
    gpio:set_level(GPIO, 14, 1),
    timer:sleep(50),

    SPI = spi:open(SPISettings),

    % Check version
    RegVersionResult = read_register(SPI, ?REG_VERSION),
    case RegVersionResult of
        {ok, 16#12} ->
            sleep(SPI),
            {ok, SPI};
        {ok, UnexpectedVersion} ->
            {error, unexpected_version, UnexpectedVersion}
    end.

init_regs(SPI) ->
    set_frequency(SPI, 868),
    write_register(SPI, ?REG_FIFO_TX_BASE_ADDR, 0),
    write_register(SPI, ?REG_FIFO_RX_BASE_ADDR, 0),
    {ok, LNA} = read_register(SPI, ?REG_LNA),
    write_register(SPI, ?REG_LNA, LNA bor 16#03),
    write_register(SPI, ?REG_MODEM_CONFIG_3, ?AUTO_AGC_FLAG),
    set_tx_power(SPI, 14),
    set_spreading_factor(SPI, 11),
    set_signal_bandwidth(SPI, 125000),
    set_sync_word(SPI, 16#34),
    enable_crc(SPI),
    idle(SPI).

broadcast_packet(SPI, Data) ->
    % prepare transmit
    idle(SPI),
    set_explicit_header_mode(SPI),
    write_register(SPI, ?REG_FIFO_ADDR_PTR, 0),
    write_register(SPI, ?REG_PAYLOAD_LENGTH, 0),

    % write data
    {ok, CurrentLength} = read_register(SPI, 16#22),
    Len = write_packet_data(SPI, Data),
    write_register(SPI, ?REG_PAYLOAD_LENGTH, CurrentLength + Len),

    % transmit
    write_register(SPI, ?REG_OP_MODE, ?MODE_LONG_RANGE_MODE bor ?MODE_TX),
    wait_flags(SPI, ?REG_IRQ_FLAGS, ?IRQ_TX_DONE_MASK),
    write_register(SPI, ?REG_IRQ_FLAGS, ?IRQ_TX_DONE_MASK).

write_packet_data(SPI, L) ->
    write_packet_data(SPI, L, 0).

write_packet_data(_SPI, [], Len) ->
    Len;
write_packet_data(SPI, [H | T], Len) ->
    write_register(SPI, ?REG_FIFO, H),
    write_packet_data(SPI, T, Len + 1).

set_explicit_header_mode(SPI) ->
    {ok, ModemConfig1} = read_register(SPI, ?REG_MODEM_CONFIG_1),
    write_register(SPI, 16#fe, ModemConfig1 band 16#fe).

idle(SPI) ->
    write_register(SPI, ?REG_OP_MODE, ?MODE_LONG_RANGE_MODE bor ?MODE_STDBY).

enable_crc(SPI) ->
    {ok, ModemConfig2} = read_register(SPI, ?REG_MODEM_CONFIG_2),
    write_register(SPI, ?REG_MODEM_CONFIG_2, ModemConfig2 bor 16#04).

set_sync_word(SPI, Word) ->
    write_register(SPI, ?REG_SYNC_WORD, Word).

set_signal_bandwidth(SPI, 125000) ->
    {ok, ModemConfig1} = read_register(SPI, ?REG_MODEM_CONFIG_1),
    write_register(SPI, ?REG_MODEM_CONFIG_1, (ModemConfig1 band 16#0f) bor (7 bsl 4)).

set_tx_power(SPI, Level) ->
    write_register(SPI, ?REG_PADAC, 16#87),
    % con PA_BOOST
    write_register(SPI, ?REG_PA_CONFIG, 16#80 bor (Level - 2)).

set_spreading_factor(SPI, SF) ->
    write_register(SPI, ?REG_DETECTION_OPTIMIZE, 16#c3),
    write_register(SPI, ?REG_DETECTION_THRESHOLD, 16#0a),
    {ok, ModemConfig2} = read_register(SPI, ?REG_MODEM_CONFIG_2),
    write_register(SPI, ?REG_MODEM_CONFIG_2, (ModemConfig2 band 16#0f) bor ((SF bsl 4) band 16#f0)).

sleep(SPI) ->
    write_register(SPI, ?REG_OP_MODE, ?MODE_LONG_RANGE_MODE bor ?MODE_SLEEP).

set_frequency(SPI, 868) ->
    F = 14221312,
    write_register(SPI, ?REG_FRF_MSB, ((F bsr 16) band 16#FF)),
    write_register(SPI, ?REG_FRF_MID, ((F bsr 8) band 16#FF)),
    write_register(SPI, ?REG_FRF_LSB, F band 16#FF).

enable_receive(SPI) ->
    write_register(SPI, ?REG_OP_MODE, ?MODE_LONG_RANGE_MODE bor ?MODE_RX_CONTINUOUS).

handle_irq(SPI) ->
    {ok, IRQFlags} = read_register(SPI, ?REG_IRQ_FLAGS),
    write_register(SPI, ?REG_IRQ_FLAGS, IRQFlags),

    if
        ((IRQFlags band ?IRQ_RX_DONE_MASK) /= 0) andalso
            ((IRQFlags band ?IRQ_PAYLOAD_CRC_ERROR_MASK) == 0) ->
            {ok, PacketLength} = read_register(SPI, ?REG_RX_NB_BYTES),
            {ok, CurrentAddr} = read_register(SPI, ?REG_FIFO_RX_CURRENT_ADDR),

            write_register(SPI, ?REG_FIFO_ADDR_PTR, CurrentAddr),
            io:format("Received data: ~s~n", [read(SPI, PacketLength)]),

            write_register(SPI, ?REG_FIFO_ADDR_PTR, 0);
        (IRQFlags band ?IRQ_RX_DONE_MASK) /= 0 ->
            io:format("CRC error");
        true ->
            io:format("Unexpected IRQ")
    end.

read(_SPI, 0) ->
    [];
read(SPI, Len) ->
    {ok, Data} = read_register(SPI, ?REG_FIFO),
    [Data | read(SPI, Len - 1)].

wait_flags(SPI, Register, Mask) ->
    wait_flags(SPI, Register, Mask, 0).

wait_flags(SPI, Register, Mask, 0) ->
    {ok, Flags} = read_register(SPI, Register),
    wait_flags(SPI, Register, Mask, Flags band Mask);
wait_flags(_SPI, _Register, _Mask, _NotZero) ->
    ok.

read_register(SPI, Address) ->
    spi:read_at(SPI, my_device, Address, 8).

write_register(SPI, Address, Data) ->
    spi:write_at(SPI, my_device, Address, 8, Data).
