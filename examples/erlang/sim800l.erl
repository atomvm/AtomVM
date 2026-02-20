%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%%-----------------------------------------------------------------------------
%% @doc SIM800L AT command demo.
%%
%% Opens a UART connection to a SIM800L GSM module and verifies it responds
%% to basic AT commands. Prints firmware identification and signal quality
%% every 10 seconds.
%%
%% Be careful: SIM800L boards can draw up to 2A and shouldn't be powered by
%% the 3.3V of the usual Pico / ESP32 boards. It's ok for this demo but
%% do not put a SIM card in them to avoid damaging your board.
%%
%% The SIM800L communicates at 115200 baud (8N1) by default.
%%
%% Default pins are auto-detected from the platform and chip model:
%%
%% Pico (UART1):            TX=GP4, RX=GP5
%% ESP32/S2/S3 (UART1):     TX=17,  RX=16
%% ESP32-C3/C5 (UART1):     TX=4,   RX=5
%% ESP32-C6/C61 (UART1):    TX=4,   RX=5
%% @end
%%-----------------------------------------------------------------------------
-module(sim800l).
-export([start/0]).

-define(AT_TIMEOUT, 2000).

start() ->
    {TX, RX} = default_pins(),
    io:format("Opening UART1 on TX=~B RX=~B~n", [TX, RX]),
    UART = uart:open("UART1", [
        {tx, TX},
        {rx, RX},
        {speed, 115200}
    ]),
    %% SIM800L takes 3-5 seconds to boot after power-on
    case wait_for_module(UART, 5) of
        ok ->
            io:format("SIM800L responding to AT commands~n"),
            at_identify(UART),
            loop(UART);
        error ->
            io:format("SIM800L not responding, giving up~n"),
            uart:close(UART)
    end.

loop(UART) ->
    at_signal_quality(UART),
    timer:sleep(10000),
    loop(UART).

wait_for_module(_UART, 0) ->
    error;
wait_for_module(UART, Retries) ->
    drain(UART),
    case at_command(UART, <<"AT">>) of
        {ok, _} ->
            ok;
        {error, _} ->
            timer:sleep(1000),
            wait_for_module(UART, Retries - 1)
    end.

at_identify(UART) ->
    case at_command(UART, <<"ATI">>) of
        {ok, Response} ->
            io:format("Module info: ~s~n", [Response]);
        {error, Reason} ->
            io:format("ATI failed: ~p~n", [Reason])
    end.

at_signal_quality(UART) ->
    case at_command(UART, <<"AT+CSQ">>) of
        {ok, Response} ->
            io:format("Signal quality: ~s~n", [Response]);
        {error, Reason} ->
            io:format("AT+CSQ failed: ~p~n", [Reason])
    end.

%%-----------------------------------------------------------------------------
%% @private Send an AT command and collect the response until OK or ERROR.
%%-----------------------------------------------------------------------------
at_command(UART, Command) ->
    uart:write(UART, [Command, <<"\r\n">>]),
    collect_response(UART, []).

collect_response(UART, Acc) ->
    case uart:read(UART, ?AT_TIMEOUT) of
        {ok, Data} ->
            NewAcc = [Data | Acc],
            Combined = erlang:iolist_to_binary(lists:reverse(NewAcc)),
            case parse_response(Combined) of
                {ok, Body} -> {ok, Body};
                error -> {error, Combined};
                incomplete -> collect_response(UART, NewAcc)
            end;
        {error, timeout} when Acc =/= [] ->
            Combined = erlang:iolist_to_binary(lists:reverse(Acc)),
            case parse_response(Combined) of
                {ok, Body} -> {ok, Body};
                _ -> {error, {partial, Combined}}
            end;
        {error, timeout} ->
            {error, timeout}
    end.

%% Look for OK or ERROR in the accumulated response
parse_response(Data) ->
    case binary:match(Data, <<"\r\nOK\r\n">>) of
        {_Pos, _Len} ->
            Body = strip_status(Data),
            {ok, Body};
        nomatch ->
            case binary:match(Data, <<"\r\nERROR\r\n">>) of
                {_Pos2, _Len2} -> error;
                nomatch -> incomplete
            end
    end.

%% Extract body between echo/first CRLF and final status line
strip_status(Data) ->
    Trimmed = trim_leading_crlf(Data),
    case binary:match(Trimmed, <<"\r\nOK\r\n">>) of
        {Pos, _} -> binary:part(Trimmed, 0, Pos);
        nomatch -> Trimmed
    end.

trim_leading_crlf(<<"\r\n", Rest/binary>>) -> trim_leading_crlf(Rest);
trim_leading_crlf(Data) -> Data.

%% Discard any pending data in the UART buffer
drain(UART) ->
    case uart:read(UART, 100) of
        {ok, _} -> drain(UART);
        {error, timeout} -> ok
    end.

%%-----------------------------------------------------------------------------
%% Platform-specific default pins
%%-----------------------------------------------------------------------------
default_pins() ->
    default_pins(atomvm:platform()).

%%         {TX, RX}
default_pins(pico) -> {4, 5};
default_pins(esp32) -> esp32_default_pins().

esp32_default_pins() ->
    #{model := Model} = erlang:system_info(esp32_chip_info),
    esp32_default_pins(Model).

%%                           {TX, RX}
esp32_default_pins(esp32) -> {17, 16};
esp32_default_pins(esp32_s2) -> {17, 16};
esp32_default_pins(esp32_s3) -> {17, 16};
esp32_default_pins(esp32_c2) -> {4, 5};
esp32_default_pins(esp32_c3) -> {4, 5};
esp32_default_pins(esp32_c5) -> {4, 5};
esp32_default_pins(esp32_c6) -> {4, 5};
esp32_default_pins(esp32_c61) -> {4, 5};
esp32_default_pins(_) -> {17, 16}.
