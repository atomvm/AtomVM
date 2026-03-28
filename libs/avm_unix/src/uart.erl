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

%%-----------------------------------------------------------------------------
%% @doc UART implementation for Unix using POSIX termios.
%%
%% This module implements the {@link uart_hal} behaviour for Unix platforms
%% using POSIX file descriptors and termios configuration.
%%
%% The peripheral name is a device path such as `"/dev/ttyUSB0"' or
%% `"/dev/cu.usbserial-1234"'.
%%
%% Example:
%% ```
%% UART = uart:open([{peripheral, "/dev/ttyUSB0"}, {speed, 115200}]),
%% ok = uart:write(UART, <<"hello">>),
%% case uart:read(UART, 1000) of
%%     {ok, Data} -> io:format("Got: ~p~n", [Data]);
%%     {error, timeout} -> io:format("No data~n")
%% end,
%% ok = uart:close(UART).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(uart).

-behaviour(uart_hal).

-export([open/1, open/2, close/1, read/1, read/2, write/2]).

-type uart_opts() :: [
    {peripheral, string() | binary()}
    | {speed, pos_integer()}
    | {data_bits, 5..8}
    | {stop_bits, 1 | 2}
    | {parity, none | even | odd}
    | {flow_control, none | hardware | software}
].

%%-----------------------------------------------------------------------------
%% @param Name device path, e.g. `"/dev/ttyUSB0"'
%% @param Opts UART configuration options
%% @returns UART handle or error
%% @doc Open a UART device with the given name and options.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Name :: string() | binary(), Opts :: uart_opts()) ->
    {pid(), atomvm:posix_fd()} | {error, term()}.
open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

%%-----------------------------------------------------------------------------
%% @param Opts UART configuration options including `{peripheral, Path}'
%% @returns UART handle or error
%% @doc Open a UART device.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Opts :: uart_opts()) -> {pid(), atomvm:posix_fd()} | {error, term()}.
open(Opts) ->
    Device =
        case proplists:get_value(peripheral, Opts) of
            undefined -> {error, {missing, peripheral}};
            D when is_binary(D) -> D;
            D when is_list(D) -> D
        end,
    case Device of
        {error, _} = Err ->
            Err;
        _ ->
            case atomvm:posix_open(Device, [o_rdwr, o_noctty]) of
                {ok, Fd} ->
                    case configure(Fd, Opts) of
                        ok ->
                            atomvm:posix_tcflush(Fd, tcioflush),
                            Pid = spawn_link(fun() ->
                                process_flag(trap_exit, true),
                                loop(Fd)
                            end),
                            {Pid, Fd};
                        {error, _} = CfgErr ->
                            atomvm:posix_close(Fd),
                            CfgErr
                    end;
                {error, _} = OpenErr ->
                    OpenErr
            end
    end.

%%-----------------------------------------------------------------------------
%% @param UART handle returned by open
%% @returns ok or error
%% @doc Close the UART device.
%% @end
%%-----------------------------------------------------------------------------
-spec close({pid(), atomvm:posix_fd()}) -> ok | {error, term()}.
close({Pid, Fd}) ->
    MRef = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 5000 ->
        demonitor(MRef, [flush])
    end,
    atomvm:posix_close(Fd).

%%-----------------------------------------------------------------------------
%% @param UART handle
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc Blocking read — waits until data is available.
%% @end
%%-----------------------------------------------------------------------------
-spec read({pid(), atomvm:posix_fd()}) -> {ok, binary()} | {error, term()}.
read({Pid, _Fd}) ->
    Ref = make_ref(),
    Pid ! {read, self(), Ref, infinity},
    receive
        {Ref, Result} -> Result
    end.

%%-----------------------------------------------------------------------------
%% @param UART handle
%% @param Timeout in milliseconds
%% @returns `{ok, Data}' or `{error, timeout}' or `{error, Reason}'
%% @doc Read with timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec read({pid(), atomvm:posix_fd()}, pos_integer() | infinity) ->
    {ok, binary()} | {error, term()}.
read(UART, infinity) ->
    read(UART);
read({Pid, _Fd}, Timeout) ->
    Ref = make_ref(),
    Pid ! {read, self(), Ref, Timeout},
    receive
        {Ref, Result} -> Result
    after Timeout + 100 ->
        {error, timeout}
    end.

%%-----------------------------------------------------------------------------
%% @param UART handle
%% @param Data to write
%% @returns ok or error
%% @doc Write data to the UART.
%% @end
%%-----------------------------------------------------------------------------
-spec write({pid(), atomvm:posix_fd()}, iodata()) -> ok | {error, term()}.
write({_Pid, Fd}, Data) ->
    Bin = iolist_to_binary(Data),
    write_loop(Fd, Bin).

%% Internal: write all bytes
write_loop(_Fd, <<>>) ->
    ok;
write_loop(Fd, Bin) ->
    case atomvm:posix_write(Fd, Bin) of
        {ok, N} ->
            <<_:N/binary, Rest/binary>> = Bin,
            write_loop(Fd, Rest);
        {error, _} = Err ->
            Err
    end.

%%--------------------------------------------------------------------
%% Internal: async reader loop using posix_select_read
%%--------------------------------------------------------------------

loop(Fd) ->
    receive
        {read, From, Ref, Timeout} ->
            %% Try immediate read first
            case atomvm:posix_read(Fd, 256) of
                {ok, Data} ->
                    From ! {Ref, {ok, Data}},
                    loop(Fd);
                {error, eagain} ->
                    %% No data — use select to wait
                    ok = atomvm:posix_select_read(Fd, self(), Ref),
                    TimerRef =
                        case Timeout of
                            infinity -> undefined;
                            Ms -> erlang:send_after(Ms, self(), {timeout, Ref})
                        end,
                    wait_data(Fd, From, Ref, TimerRef);
                eof ->
                    From ! {Ref, {error, closed}},
                    loop(Fd);
                {error, _} = Err ->
                    From ! {Ref, Err},
                    loop(Fd)
            end;
        {'EXIT', _From, _Reason} ->
            catch atomvm:posix_select_stop(Fd),
            ok
    end.

wait_data(Fd, From, Ref, TimerRef) ->
    receive
        {select, _FdRes, Ref, ready_input} ->
            cancel_timer(TimerRef),
            case atomvm:posix_read(Fd, 256) of
                {ok, Data} ->
                    From ! {Ref, {ok, Data}};
                {error, eagain} ->
                    From ! {Ref, {error, timeout}};
                eof ->
                    From ! {Ref, {error, closed}};
                {error, _} = Err ->
                    From ! {Ref, Err}
            end,
            loop(Fd);
        {timeout, Ref} ->
            atomvm:posix_select_stop(Fd),
            From ! {Ref, {error, timeout}},
            loop(Fd);
        {'EXIT', _From, _Reason} ->
            cancel_timer(TimerRef),
            atomvm:posix_select_stop(Fd),
            ok
    end.

cancel_timer(undefined) -> ok;
cancel_timer(Ref) -> erlang:cancel_timer(Ref).

%%--------------------------------------------------------------------
%% Internal: termios configuration
%%--------------------------------------------------------------------

configure(Fd, Opts) ->
    Speed = proplists:get_value(speed, Opts, 115200),
    DataBits = proplists:get_value(data_bits, Opts, 8),
    StopBits = proplists:get_value(stop_bits, Opts, 1),
    Parity = proplists:get_value(parity, Opts, none),
    FlowControl = proplists:get_value(flow_control, Opts, none),
    %% tcsetattr with raw => true applies cfmakeraw in C (portable).
    %% High-level options (data_bits, parity, etc.) are also applied in C.
    atomvm:posix_tcsetattr(Fd, tcsanow, #{
        raw => true,
        ispeed => Speed,
        ospeed => Speed,
        data_bits => DataBits,
        stop_bits => StopBits,
        parity => Parity,
        flow_control => FlowControl,
        clocal => true
    }).
