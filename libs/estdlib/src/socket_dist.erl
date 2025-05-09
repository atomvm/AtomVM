%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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
-module(socket_dist).

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

% Required include headers
-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-spec listen(string()) -> {ok, {any(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name) ->
    listen(Name, 0).

-spec listen(string(), non_neg_integer()) ->
    {ok, {any(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name, SocketPort) ->
    {ok, LSock} = socket:open(inet, stream, tcp),
    case
        socket:bind(LSock, #{
            family => inet,
            port => SocketPort,
            addr => {0, 0, 0, 0}
        })
    of
        ok ->
            ok = socket:listen(LSock),
            {ok, #{addr := Addr, port := Port}} = socket:sockname(LSock),
            ErlEpmd = net_kernel:epmd_module(),
            Address = #net_address{
                host = Addr,
                protocol = tcp,
                family = inet
            },
            case ErlEpmd:register_node(Name, Port) of
                {ok, Creation} ->
                    {ok, {LSock, Address, Creation}};
                Error ->
                    socket:close(LSock),
                    Error
            end;
        {error, _} = Error ->
            socket:close(LSock),
            Error
    end.

-spec address() -> #net_address{}.
address() ->
    #net_address{
        host = {0, 0, 0, 0},
        protocol = tcp,
        family = inet
    }.

-spec accept(any()) -> pid().
accept(Listen) ->
    Kernel = self(),
    spawn_link(fun() -> accept_loop(Kernel, Listen) end).

accept_loop(Kernel, LSock) ->
    case socket:accept(LSock) of
        {ok, CSock} ->
            {ok, DistController} = socket_dist_controller:start(CSock),
            Kernel ! {accept, self(), DistController, inet, tcp},
            receive
                {Kernel, controller, SupervisorPid} ->
                    true = socket_dist_controller:supervisor(DistController, SupervisorPid);
                {Kernel, unsupported_protocol} ->
                    exit(unsupported_protocol)
            end,
            accept_loop(Kernel, LSock);
        {error, _} = Error ->
            exit(Error)
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
        f_send = fun socket_dist_controller:send/2,
        f_recv = fun socket_dist_controller:recv/3,
        f_setopts_pre_nodeup = fun socket_dist_controller:setopts_pre_nodeup/1,
        f_setopts_post_nodeup = fun socket_dist_controller:setopts_post_nodeup/1,
        f_getll = fun socket_dist_controller:getll/1,
        f_address = fun socket_dist_controller:address/2,
        f_handshake_complete = fun socket_dist_controller:handshake_complete/3,
        mf_tick = fun socket_dist_controller:tick/1,
        mf_getstat = fun socket_dist_controller:getstat/1,
        request_type = Type
    }.

setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    spawn_opt(
        fun() -> do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) end,
        dist_util:net_ticker_spawn_options()
    ).

do_setup(Kernel, Node, Type, MyNode, _LongNames, SetupTime) ->
    case string:split(atom_to_list(Node), "@") of
        [Name, Host] ->
            Timer = dist_util:start_timer(SetupTime),
            case inet:getaddr(Host, inet) of
                {ok, Ip} ->
                    dist_util:reset_timer(Timer),
                    ErlEpmd = net_kernel:epmd_module(),
                    case ErlEpmd:port_please(Name, Ip) of
                        {port, TcpPort, Version} ->
                            dist_util:reset_timer(Timer),
                            {ok, Sock} = socket:open(inet, stream, tcp),
                            case
                                socket:connect(Sock, #{family => inet, addr => Ip, port => TcpPort})
                            of
                                ok ->
                                    {ok, DistController} = socket_dist_controller:start(Sock),
                                    true = socket_dist_controller:supervisor(
                                        DistController, self()
                                    ),
                                    HSData = hs_data(
                                        Kernel,
                                        MyNode,
                                        DistController,
                                        [],
                                        Node,
                                        Version,
                                        Type,
                                        Timer
                                    ),
                                    dist_util:handshake_we_started(HSData);
                                Other1 ->
                                    ?shutdown2(Node, {unexpected, Other1})
                            end;
                        Other2 ->
                            ?shutdown2(Node, {unexpected, Other2})
                    end;
                Other3 ->
                    ?shutdown2(Node, {unexpected, Other3})
            end;
        Other4 ->
            ?shutdown2(Node, {unexpected, Other4})
    end.

close(Listen) ->
    socket:close(Listen).

select(_Node) ->
    true.
