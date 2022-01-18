%%-----------------------------------------------------------------------------
%% @doc AtomvVM I2c interface
%%
%% This module provides and interface into the AtomVM I2C driver.
%%
%% Use this module to communicate with devices connected to your ESP32
%% device via the 2-wire I2C interface.
%%
%% Using this interface, you can read or write data to an I2C device
%% at a given I2C address.  In addition, you may read from or write to
%% specific registers on the I2C device.
%% @end
%%-----------------------------------------------------------------------------
-module(i2c).
-export([
    open/1, begin_transmission/2, write_byte/2, end_transmission/1,
    read_bytes/3, read_bytes/4, write_bytes/2, write_bytes/3, write_bytes/4
]).

-type pin() :: non_neg_integer().
-type freq_hz() :: non_neg_integer().
-type param() :: {scl_io_num, pin()} | {sda_io_num, pin()} | {i2c_clock_hz, freq_hz()}.
-type params() :: [param()].
-type i2c() :: pid().
-type address() :: non_neg_integer().
-type register() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% @param   Param Initialization parameters
%% @returns process id of the driver.
%% @doc     Open a connection to the I2C driver
%%
%%          This function will open a connection to the I2C driver.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Param::params()) -> i2c().
open(Param) ->
    open_port({spawn, "i2c"}, Param).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @returns `ok' or `error'
%% @doc     Begin a transimission of I2C commands
%%
%%          This command is typically followed by one or more calls to
%%          `write_byte/2' and then a call to `end_transmission/1'
%% @end
%%-----------------------------------------------------------------------------
-spec begin_transmission(I2c::i2c(), Address::address()) -> ok | error.
begin_transmission(I2C, Address) ->
    port:call(I2C, {begin_transmission, Address}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Byte value to write
%% @returns `ok' or `error'
%% @doc     Write a byte to the device.
%%
%%          This command must be wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_byte(I2c::i2c(), Byte::byte()) -> ok | error.
write_byte(I2C, Byte) ->
    port:call(I2C, {write_byte, Byte}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Byte value to write
%% @returns `ok' or `error'
%% @doc     Write a sequence of bytes to the device.
%%
%%          This command must be wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2c::i2c(), Bytes::binary()) -> ok | error.
write_bytes(I2C, Bytes) ->
    port:call(I2C, {write_bytes, Bytes}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @returns `ok' or `error'
%% @doc     End a transimission of I2C commands
%%
%%          This command is typically preceeded by a call to `begin_transmission/2'
%%          and one or more calls to `write_byte/2'.
%% @end
%%-----------------------------------------------------------------------------
-spec end_transmission(I2c::i2c()) -> ok | error.
end_transmission(I2C) ->
    port:call(I2C, {end_transmission}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   Count The number of bytes to read
%% @returns the read binary data or `error'
%% @doc     Read a block of bytes from the I2C device.
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(I2c::i2c(), Address::address(), Count:: non_neg_integer()) -> error | binary().
read_bytes(I2C, Address, Count) ->
    port:call(I2C, {read_bytes, Address, Count}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   Register The register address in the device from which to read data
%% @param   Count The number of bytes to read
%% @returns the read binary data or `error'
%% @doc     Read a block of bytes from the I2C device starting at a specified
%%          register address
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec read_bytes(I2c::i2c(), Address::address(), Register::register(), Count:: non_neg_integer()) -> error | binary().
read_bytes(I2C, Address, Register, Count) ->
    port:call(I2C, {read_bytes, Address, Count, Register}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   BinOrInt The binary or byte value to write
%% @returns `ok' or `error'
%% @doc     Write a block of bytes to the I2C device.
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2c::i2c(), Address::address(), BinOrInt :: binary() | byte()) -> ok | error.
write_bytes(I2C, Address, BinOrInt) ->
    port:call(I2C, {write_bytes, Address, BinOrInt}).

%%-----------------------------------------------------------------------------
%% @param   I2C I2C instance created via `open/1'
%% @param   Address I2C Address of the device (typically fixed for the device type)
%% @param   Register The register address in the device to which to write data
%% @param   BinOrInt The binary or byte value to write
%% @returns `ok' or `error'
%% @doc     Write a block of bytes to the I2C device starting at a specified
%%          register address.
%%
%%          This command is not wrapped in a `begin_transmission/2'
%%          and `end_transmission/1' call.
%% @end
%%-----------------------------------------------------------------------------
-spec write_bytes(I2c::i2c(), Address::address(), Register::register(), BinOrInt :: binary() | integer()) -> ok | error.
write_bytes(I2C, Address, Register, BinOrInt) ->
    port:call(I2C, {write_bytes, Address, BinOrInt, Register}).
