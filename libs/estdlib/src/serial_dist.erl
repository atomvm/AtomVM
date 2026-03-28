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
%% @doc Distribution over serial (UART) connections.
%%
%% This module implements the Erlang distribution protocol over UART,
%% enabling distributed Erlang between devices connected via serial lines.
%% This is particularly useful for microcontrollers that lack WiFi/TCP
%% (e.g. STM32) but have UART available.
%%
%% <h3>Multi-port support</h3>
%%
%% Many MCUs have multiple UARTs (e.g. UART0 for console, UART1 and UART2
%% for peer connections). This module supports opening several serial ports,
%% each connecting to a different peer node. Pass a list of UART
%% configurations via the `uart_ports' option:
%%
%% ```
%% {ok, _} = net_kernel:start('mynode@serial.local', #{
%%     name_domain => longnames,
%%     proto_dist => serial_dist,
%%     avm_dist_opts => #{
%%         uart_ports => [
%%             [{peripheral, "UART1"}, {speed, 115200}, {tx, 17}, {rx, 16}],
%%             [{peripheral, "UART2"}, {speed, 115200}, {tx, 4}, {rx, 5}]
%%         ],
%%         uart_module => uart
%%     }
%% }).
%% '''
%%
%% For a single port, the `uart_opts' key can also be used:
%%
%% ```
%% avm_dist_opts => #{
%%     uart_opts => [{peripheral, "UART1"}, {speed, 115200}]
%% }
%% '''
%%
%% <h3>Peer-to-peer model</h3>
%%
%% Each UART handles exactly one connection. A coordinator process
%% manages one link manager per UART port. Each link manager owns all
%% reads from its UART and arbitrates between incoming connections
%% (accept) and outgoing connections (setup).
%%
%% The link managers periodically send sync markers (`<<16#AA, 16#55>>')
%% on their UART so the peer can detect we are alive. When a peer wants
%% to connect, it sends a preamble of repeated sync markers followed by a
%% framed handshake packet. Each frame on the wire uses the format:
%%
%% ```
%% <<16#AA, 16#55, Length, Payload, CRC32:32>>
%% '''
%%
%% The CRC32 covers the Length and Payload bytes. False sync matches
%% (where `<<16#AA, 16#55>>' appears in payload data) are rejected by
%% maximum frame size checks and CRC verification. On CRC failure
%% during an active connection, the connection is torn down. During
%% pre-connection scanning, the buffer is discarded and retried.
%%
%% <h3>BEAM compatibility</h3>
%%
%% This module also works on BEAM (standard Erlang/OTP). On BEAM, start
%% the VM with `-proto_dist serial' and configure UART options via
%% application environment before calling `net_kernel:start/1':
%%
%% ```
%% application:set_env(serial_dist, dist_opts, #{
%%     uart_opts => [{peripheral, "/dev/ttyUSB0"}, {speed, 115200}],
%%     uart_module => my_uart_module
%% }).
%% net_kernel:start(['mynode@serial.local', longnames]).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(serial_dist).

% dist interface
-export([
    listen/1,
    listen/2,
    accept/1,
    accept_connection/5,
    setup/5,
    close/1,
    select/1,
    address/0
]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-define(SYNC_MAGIC, <<16#AA, 16#55>>).

-type uart_handle() :: pid() | port().
-type listen_handle() :: nonempty_list({uart_handle(), module()}).

-spec listen(string()) -> {ok, {listen_handle(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name) ->
    listen(Name, #{}).

-spec listen(string(), map() | string()) ->
    {ok, {listen_handle(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name, Opts) when is_map(Opts) ->
    %% BEAM OTP 25+ may also pass an empty map.
    %% Merge with application env to pick up BEAM-side config.
    EnvOpts = beam_env_opts(),
    EffectiveOpts = maps:merge(EnvOpts, Opts),
    listen_impl(Name, EffectiveOpts);
listen(Name, _Host) ->
    %% BEAM path (OTP < 25): Host is a hostname string.
    listen_impl(Name, beam_env_opts()).

beam_env_opts() ->
    case application:get_env(serial_dist, dist_opts) of
        {ok, O} when is_map(O) -> O;
        _ -> #{}
    end.

listen_impl(_Name, Opts) ->
    UartMod = maps:get(uart_module, Opts, uart),
    UartConfigs =
        case maps:find(uart_ports, Opts) of
            {ok, Ports} ->
                Ports;
            error ->
                case maps:find(uart_opts, Opts) of
                    {ok, UartOpts} ->
                        [UartOpts];
                    error ->
                        %% Fallback: try SERIAL_DEVICE env var
                        case os:getenv("SERIAL_DEVICE") of
                            false ->
                                [[{peripheral, "UART1"}, {speed, 115200}]];
                            Device ->
                                [[{peripheral, Device}, {speed, 115200}]]
                        end
                end
        end,
    OpenPorts = lists:filtermap(
        fun(UartOpts) ->
            case UartMod:open(UartOpts) of
                {error, Reason} ->
                    io:format("serial_dist: failed to open UART ~p: ~p~n", [UartOpts, Reason]),
                    false;
                UartPort ->
                    {true, {UartPort, UartMod}}
            end
        end,
        UartConfigs
    ),
    case OpenPorts of
        [] ->
            {error, no_uart_ports};
        _ ->
            Address = #net_address{
                address = serial,
                host = serial,
                protocol = serial,
                family = serial
            },
            {ok, {OpenPorts, Address, 1}}
    end.

-spec address() -> #net_address{}.
address() ->
    #net_address{
        address = serial,
        host = serial,
        protocol = serial,
        family = serial
    }.

%% @doc Called by `net_kernel' after `listen/1' succeeds.
%% Spawns a coordinator process and one link manager per UART port.
-spec accept(listen_handle() | {uart_handle(), module()}) -> pid().
accept(Ports) when is_list(Ports) ->
    Kernel = self(),
    spawn_link(fun() ->
        register(serial_dist_link_manager, self()),
        %% Trap exits so a single link manager crash doesn't
        %% tear down the coordinator and all other managers.
        process_flag(trap_exit, true),
        Coordinator = self(),
        Managers = [
            spawn_link(fun() ->
                link_manager(Coordinator, UartPort, UartMod, <<>>)
            end)
         || {UartPort, UartMod} <- Ports
        ],
        InitialStates = maps:from_list([{M, idle} || M <- Managers]),
        coordinator_loop(Kernel, InitialStates, [], #{})
    end);
accept({UartPort, UartMod}) ->
    accept([{UartPort, UartMod}]).

%%--------------------------------------------------------------------
%% Coordinator
%%
%% The coordinator routes messages between link managers and
%% the net_kernel. It tracks which managers are idle vs busy
%% (handling a connection). It:
%%   - Forwards accept notifications from link managers to kernel
%%   - Forwards controller assignments from kernel back to managers
%%   - Broadcasts setup requests to all idle managers, awards the
%%     first responder, and aborts the rest
%%--------------------------------------------------------------------

coordinator_loop(Kernel, ManagerStates, PendingAccepts, SetupAwards) ->
    receive
        %% A link manager detected an incoming connection
        {link_accept, ManagerPid, Ctrl} ->
            Kernel ! {accept, self(), Ctrl, serial, serial},
            NewStates = maps:put(ManagerPid, busy, ManagerStates),
            coordinator_loop(Kernel, NewStates, PendingAccepts ++ [ManagerPid], SetupAwards);
        %% Kernel accepts the connection
        {Kernel, controller, SupervisorPid} ->
            case PendingAccepts of
                [ManagerPid | Rest] ->
                    ManagerPid ! {coordinator_accept, SupervisorPid},
                    coordinator_loop(Kernel, ManagerStates, Rest, SetupAwards);
                [] ->
                    io:format("serial_dist: controller assigned with no pending accepts~n"),
                    coordinator_loop(Kernel, ManagerStates, PendingAccepts, SetupAwards)
            end;
        %% Kernel rejects the connection
        {Kernel, unsupported_protocol} ->
            case PendingAccepts of
                [ManagerPid | Rest] ->
                    ManagerPid ! {coordinator_reject},
                    coordinator_loop(Kernel, ManagerStates, Rest, SetupAwards);
                [] ->
                    coordinator_loop(Kernel, ManagerStates, PendingAccepts, SetupAwards)
            end;
        %% Outgoing connection request: broadcast to all idle managers
        {setup, SetupPid} ->
            IdleManagers = [M || {M, idle} <- maps:to_list(ManagerStates)],
            case IdleManagers of
                [] ->
                    SetupPid ! {link_manager_unavailable},
                    coordinator_loop(Kernel, ManagerStates, PendingAccepts, SetupAwards);
                _ ->
                    monitor(process, SetupPid),
                    lists:foreach(fun(M) -> M ! {setup, SetupPid} end, IdleManagers),
                    NewAwards = maps:put(SetupPid, false, SetupAwards),
                    coordinator_loop(Kernel, ManagerStates, PendingAccepts, NewAwards)
            end;
        %% A link manager created a controller for an outgoing setup
        {setup_ctrl, ManagerPid, Ctrl, SetupPid} ->
            case maps:find(SetupPid, SetupAwards) of
                {ok, false} ->
                    %% First responder wins: forward controller to setup process
                    SetupPid ! {link_manager, Ctrl},
                    ManagerPid ! {setup_awarded},
                    NewStates = maps:put(ManagerPid, busy, ManagerStates),
                    NewAwards = maps:remove(SetupPid, SetupAwards),
                    coordinator_loop(Kernel, NewStates, PendingAccepts, NewAwards);
                _ ->
                    %% Already awarded or unknown: abort this attempt
                    ManagerPid ! {setup_abort},
                    coordinator_loop(Kernel, ManagerStates, PendingAccepts, SetupAwards)
            end;
        %% Setup process died before being awarded -- clean up so late
        %% responders get aborted instead of sending to a dead process.
        {'DOWN', _Ref, process, SetupPid, _Reason} ->
            NewAwards = maps:remove(SetupPid, SetupAwards),
            coordinator_loop(Kernel, ManagerStates, PendingAccepts, NewAwards);
        %% A link manager's connection ended, back to idle
        {manager_idle, ManagerPid} ->
            NewStates = maps:put(ManagerPid, idle, ManagerStates),
            coordinator_loop(Kernel, NewStates, PendingAccepts, SetupAwards);
        %% net_kernel (or accept process) died -- shut down.
        {'EXIT', Kernel, Reason} ->
            exit(Reason);
        %% A link manager crashed (UART I/O error etc.) -- remove it so
        %% the coordinator continues running for remaining managers.
        {'EXIT', ManagerPid, _Reason} ->
            NewStates = maps:remove(ManagerPid, ManagerStates),
            coordinator_loop(Kernel, NewStates, PendingAccepts, SetupAwards)
    end.

%%--------------------------------------------------------------------
%% Link manager
%%
%% One link manager per UART port. It:
%%   1. Checks mailbox for {setup, Pid} (non-blocking).
%%      If found, enters setup (initiator) path immediately.
%%   2. Sends a sync marker so the peer knows we are alive
%%   3. Reads from UART with a short timeout
%%   4. Tries to parse a framed handshake packet (scan_frame)
%%   5. If a complete or partial frame is detected -> accept path
%%   6. If only sync bytes -> loop
%%
%% On recovery after a failed handshake, stale setup messages are
%% flushed and the loop restarts cleanly.
%%--------------------------------------------------------------------

link_manager(Coordinator, UartPort, UartMod, Buffer) ->
    %% Check for setup request from coordinator
    receive
        {setup, SetupPid} ->
            do_setup_handshake(Coordinator, UartPort, UartMod, SetupPid)
    after 0 ->
        ok
    end,
    %% Send sync so peer knows we are alive
    case UartMod:write(UartPort, ?SYNC_MAGIC) of
        ok ->
            ok;
        {error, WriteReason} ->
            io:format("serial_dist: UART write error: ~p~n", [WriteReason]),
            exit({uart_write_error, WriteReason})
    end,
    %% Read from UART
    NewBuffer =
        case UartMod:read(UartPort, 500) of
            {ok, Data} ->
                <<Buffer/binary, Data/binary>>;
            {error, timeout} ->
                Buffer;
            {error, Reason} ->
                io:format("serial_dist: UART read error: ~p~n", [Reason]),
                exit({uart_read_error, Reason})
        end,
    %% Try to detect a framed handshake packet
    case serial_dist_controller:scan_frame(NewBuffer, 16) of
        {ok, _Payload, _Rest} ->
            %% Complete frame found; pass raw buffer to accept
            do_accept_handshake(Coordinator, UartPort, UartMod, NewBuffer);
        {crc_error, _} ->
            %% Bad data; discard and loop
            link_manager(Coordinator, UartPort, UartMod, <<>>);
        {need_more, Trimmed} when byte_size(Trimmed) > 4 ->
            %% Partial frame in flight; enter accept, controller will read more
            do_accept_handshake(Coordinator, UartPort, UartMod, Trimmed);
        {need_more, Trimmed} ->
            %% Only sync bytes or too little data; keep buffered
            link_manager(Coordinator, UartPort, UartMod, Trimmed)
    end.

%% Responder: handshake data arrived from peer.
do_accept_handshake(Coordinator, UartPort, UartMod, Data) ->
    {ok, Ctrl} = serial_dist_controller:start(UartPort, UartMod, Data),
    Coordinator ! {link_accept, self(), Ctrl},
    receive
        {coordinator_accept, SupervisorPid} ->
            true = serial_dist_controller:supervisor(Ctrl, SupervisorPid);
        {coordinator_reject} ->
            exit(unsupported_protocol)
    end,
    %% Monitor controller -- if handshake fails, recover.
    Ref = monitor(process, Ctrl),
    receive
        {'DOWN', Ref, process, Ctrl, _Reason} ->
            Coordinator ! {manager_idle, self()},
            flush_setup_messages(),
            link_manager(Coordinator, UartPort, UartMod, <<>>)
    end.

%% Initiator: send sync preamble, create controller, report to coordinator.
%% The coordinator awards the first manager to respond and aborts the rest
%% (when setup is broadcast to multiple idle managers).
do_setup_handshake(Coordinator, UartPort, UartMod, SetupPid) ->
    case serial_dist_controller:send_preamble(UartMod, UartPort) of
        ok -> ok;
        {error, PreambleReason} -> exit({uart_write_error, preamble, PreambleReason})
    end,
    {ok, Ctrl} = serial_dist_controller:start(UartPort, UartMod),
    Coordinator ! {setup_ctrl, self(), Ctrl, SetupPid},
    receive
        {setup_awarded} ->
            Ref = monitor(process, Ctrl),
            receive
                {'DOWN', Ref, process, Ctrl, _Reason} ->
                    Coordinator ! {manager_idle, self()},
                    flush_setup_messages(),
                    link_manager(Coordinator, UartPort, UartMod, <<>>)
            end;
        {setup_abort} ->
            exit(Ctrl, shutdown),
            flush_setup_messages(),
            link_manager(Coordinator, UartPort, UartMod, <<>>)
    end.

flush_setup_messages() ->
    receive
        {setup, _} -> flush_setup_messages()
    after 0 ->
        ok
    end.

accept_connection(_AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    Kernel = self(),
    spawn_opt(
        fun() -> do_accept_connection(Kernel, DistCtrl, MyNode, Allowed, SetupTime) end,
        dist_util:net_ticker_spawn_options()
    ).

do_accept_connection(Kernel, DistCtrl, MyNode, Allowed, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    HSData = hs_data(Kernel, MyNode, DistCtrl, Allowed, undefined, undefined, undefined, Timer),
    dist_util:handshake_other_started(HSData).

hs_data(Kernel, MyNode, DistCtrl, Allowed, OtherNode, OtherVersion, Type, Timer) ->
    #hs_data{
        kernel_pid = Kernel,
        this_node = MyNode,
        socket = DistCtrl,
        timer = Timer,
        this_flags = 0,
        allowed = Allowed,
        other_node = OtherNode,
        other_version = OtherVersion,
        f_send = fun serial_dist_controller:send/2,
        f_recv = fun serial_dist_controller:recv/3,
        f_setopts_pre_nodeup = fun serial_dist_controller:setopts_pre_nodeup/1,
        f_setopts_post_nodeup = fun serial_dist_controller:setopts_post_nodeup/1,
        f_getll = fun serial_dist_controller:getll/1,
        f_address = fun serial_dist_controller:address/2,
        f_handshake_complete = fun serial_dist_controller:handshake_complete/3,
        mf_tick = fun serial_dist_controller:tick/1,
        mf_getstat = fun serial_dist_controller:getstat/1,
        request_type = Type
    }.

%% @doc Called by `net_kernel' to initiate an outgoing connection.
%% Forwards the request to the coordinator which broadcasts to idle
%% link managers.
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    spawn_opt(
        fun() -> do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) end,
        dist_util:net_ticker_spawn_options()
    ).

do_setup(Kernel, Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    case whereis(serial_dist_link_manager) of
        Mgr when is_pid(Mgr) ->
            Ref = monitor(process, Mgr),
            Mgr ! {setup, self()},
            receive
                {link_manager, DistController} ->
                    demonitor(Ref, [flush]),
                    dist_util:reset_timer(Timer),
                    true = serial_dist_controller:supervisor(DistController, self()),
                    HSData = hs_data(
                        Kernel, MyNode, DistController, [], Node, 6, Type, Timer
                    ),
                    dist_util:handshake_we_started(HSData);
                {link_manager_unavailable} ->
                    demonitor(Ref, [flush]),
                    ?shutdown2(Node, no_link_managers_available);
                {'DOWN', Ref, process, Mgr, _Reason} ->
                    ?shutdown2(Node, link_manager_died)
            after SetupTime ->
                demonitor(Ref, [flush]),
                ?shutdown2(Node, no_link_manager_response)
            end;
        _ ->
            ?shutdown2(Node, no_link_manager)
    end.

close(Ports) when is_list(Ports) ->
    lists:foreach(fun({UartPort, UartMod}) -> UartMod:close(UartPort) end, Ports);
close({UartPort, UartMod}) ->
    UartMod:close(UartPort);
close(_) ->
    ok.

select(_Node) ->
    true.
