%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2019 by Davide Bettio <davide@uninstall.it>                 %
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

-module(spi).
-export([open/1, read_at/3, write_at/4]).

-type bus_config() :: [
    {miso_io_num, non_neg_integer()} |
    {mosi_io_num, non_neg_integer()} |
    {sclk_io_num, non_neg_integer()}
].
-type device_config() :: [
    {clock_speed_hz, non_neg_integer()} |
    {mode, non_neg_integer()} |
    {spi_cs_io_num, non_neg_integer()} |
    {address_len_bits, non_neg_integer()}
].
-type param() :: {bus_config, bus_config()} | {device_config, device_config()}.
-type params() :: [param()].
-type spi() :: pid().
-type address() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% @param   Params Initialization parameters
%% @returns process id of the driver.
%% @doc     Open a connection to the SPI driver
%%
%%          This function will open a connection to the SPI driver.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Params::params()) -> spi().
open(Params) ->
    open_port({spawn, "spi"}, Params).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   Address SPI Address from which to read
%% @returns `{ok, Value}' or `error'
%% @doc     Read a value from and address on the device.
%% @end
%%-----------------------------------------------------------------------------
-spec read_at(SPI::spi(), Address::address(), Len::non_neg_integer()) -> {ok, integer()} | error.
read_at(SPI, Address, Len) ->
    port:call(SPI, {read_at, Address, Len}).

%%-----------------------------------------------------------------------------
%% @param   SPI SPI instance created via `open/1'
%% @param   Address SPI Address to which to write
%% @returns `{ok, Value}' or `error'
%% @doc     Write a value to and address on the device.
%%
%%          The value returned from this function is dependent on the device and address.
%%          Consult the documentation for the device to understand expected return
%%          values from this function.
%% @end
%%-----------------------------------------------------------------------------
-spec write_at(SPI::spi(), Address::address(), Len::non_neg_integer(), Data::integer()) -> {ok, integer()} | error.
write_at(SPI, Address, Len, Data) ->
    port:call(SPI, {write_at, Address bor 16#80, Len, Data}).
