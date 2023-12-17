%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Davide Bettio <davide@uninstall.it>
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

-module(uart).
-export([open/1, open/2, close/1, read/1, write/2]).

open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

open(Opts) ->
    open_port({spawn, "uart"}, migrate_config(Opts)).

close(Pid) ->
    port:call(Pid, close).

read(Pid) ->
    port:call(Pid, read).

write(Pid, B) ->
    case is_iolist(B) of
        true ->
            port:call(Pid, {write, B});
        false ->
            throw(badarg)
    end.

%% @private
is_iolist([]) ->
    true;
is_iolist(B) when is_binary(B) ->
    true;
is_iolist(I) when is_integer(I) andalso 0 =< I andalso I =< 255 ->
    true;
is_iolist([H | T]) ->
    case is_iolist(H) of
        true ->
            is_iolist(T);
        false ->
            false
    end;
is_iolist(_) ->
    false.

migrate_config([]) ->
    [];
migrate_config([{K, V} | T]) ->
    NewK = rename_key(K),
    warn_deprecated(K, NewK),
    NewV = migrate_value(NewK, V),
    [{NewK, NewV} | migrate_config(T)].

migrate_value(peripheral, Peripheral) ->
    validate_peripheral(Peripheral);
migrate_value(_K, V) ->
    V.

rename_key(Key) ->
    case Key of
        rx_pin -> rx;
        tx_pin -> tx;
        rts_pin -> rts;
        cts_pin -> cts;
        Any -> Any
    end.

warn_deprecated(Key, Key) ->
    ok;
warn_deprecated(OldKey, NewKey) ->
    io:format("UART: found deprecated ~p, use ~p instead!!!~n", [OldKey, NewKey]).

validate_peripheral(I) when is_integer(I) ->
    io:format("UART: deprecated integer peripheral is used.~n"),
    I;
validate_peripheral([$u, $a, $r, $t | N] = Value) ->
    try list_to_integer(N) of
        % Internally integers are still used
        % TODO: change this as soon as ESP32 code is reworked
        I -> I
    catch
        error:_ -> {bardarg, {peripheral, Value}}
    end;
validate_peripheral(<<"uart", N/binary>> = Value) ->
    try binary_to_integer(N) of
        I -> I
    catch
        error:_ -> {bardarg, {peripheral, Value}}
    end;
validate_peripheral(Value) ->
    throw({bardarg, {peripheral, Value}}).
