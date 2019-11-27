-module(i2c).
-export([open/1, begin_transmission/2, write_byte/2, end_transmission/1, read_bytes/3]).

open(Param) ->
    open_port({spawn, "i2c"}, Param).

begin_transmission(I2C, Address) ->
    call(I2C, begin_transmission, Address).

write_byte(I2C, Byte) ->
    call(I2C, write_byte, Byte).

end_transmission(I2C) ->
    call(I2C, end_transmission).

read_bytes(I2C, Address, Count) ->
    call(I2C, read_bytes, Address, Count).

call(I2C, Call) ->
    I2C ! {self(), make_ref(), {Call}},
    receive
        Ret ->
            Ret
    end.

call(I2C, Call, A) ->
    I2C ! {self(), make_ref(), {Call, A}},
    receive
        Ret ->
            Ret
    end.

call(I2C, Call, A, B) ->
    I2C ! {self(), make_ref(), {Call, A, B}},
    receive
        Ret ->
            Ret
    end.
