%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-----------------------------------------------------------------------------
%% @doc ESP32-specific APIs
%%
%% This module contains functions that are specific to the ESP32 platform.
%% @end
%%-----------------------------------------------------------------------------
-module(esp).

-export([random/0, random_bytes/1, restart/0, reset_reason/0]).

-type esp_reset_reason() :: 
    esp_rst_unknown |
    esp_rst_poweron |
    esp_rst_ext |
    esp_rst_sw |
    esp_rst_panic |
    esp_rst_int_wdt |
    esp_rst_task_wdt |
    esp_rst_wdt |
    esp_rst_deepsleep |
    esp_rst_brownout |
    esp_rst_sdio.

%%-----------------------------------------------------------------------------
%% @returns random 32-bit integer.
%% @doc     Returns a random 32-bit integer value.
%%          This function will use the ESP32 hardware RNG if Bluetooth or WIFI
%%          is enabled.  Otherwise, the random value is generated using a PRNG.
%% @end
%%-----------------------------------------------------------------------------
-spec random() -> integer().
random() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Len non-negative integer
%% @returns Binary containing random sequence of bytes of length Len.
%% @doc     Returns a binary containing random sequence of bytes of length Len.
%%          Supplying a negative value will result in a badarg error.
%%          This function will use the ESP32 hardware RNG if Bluetooth or WIFI
%%          is enabled.  Otherwise, the random value is generated using a PRNG.
%% @end
%%-----------------------------------------------------------------------------
-spec random_bytes(Len::non_neg_integer()) -> binary().
random_bytes(_Len) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @doc     Restarts the ESP device
%% @end
%%-----------------------------------------------------------------------------
-spec restart() -> ok.
restart() ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @returns the reason for the restart
%% @doc     Returns the reason for the restart
%% @end
%%-----------------------------------------------------------------------------
-spec reset_reason() -> esp_reset_reason().
reset_reason() ->
    throw(nif_error).
