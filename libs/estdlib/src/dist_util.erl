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
-module(dist_util).

% This implementation is based on:
% Erlang OTP/27 distribution protocol documentation
% https://www.erlang.org/doc/apps/erts/erl_dist_protocol
% Erlang OTP/27 implementation
% https://github.com/erlang/otp/blob/maint-27/lib/kernel/src/dist_util.erl

% The main differences with Erlang OTP/27 are as follows:
% - this implementation only accept latest protocol (epmd 6 and flags from OTP-26)
% - this implementation doesn't support named flag
% - net_kernel is responsible for handling cookies
% - interface with net_kernel is done through functions and eventually gen_server API
% - receive function shall return binaries and not lists

% Public API
-export([
    net_ticker_spawn_options/0,
    start_timer/1,
    cancel_timer/1,
    reset_timer/1,
    handshake_other_started/1,
    handshake_we_started/1,
    shutdown/3,
    shutdown/4
]).

% Required include headers
-include_lib("kernel/include/dist_util.hrl").

% Because this module can be compiled with OTP21 headers, we need to define
% flags that we support and that OTP-21 doesn't know. For this reason, we don't
% include dist.hrl but instead define the flags here.

% OTP-21:
-define(DFLAG_PUBLISHED, 1).
-define(DFLAG_ATOM_CACHE, 2).
-define(DFLAG_EXTENDED_REFERENCES, 4).
-define(DFLAG_DIST_MONITOR, 8).
-define(DFLAG_FUN_TAGS, 16#10).
-define(DFLAG_DIST_MONITOR_NAME, 16#20).
-define(DFLAG_HIDDEN_ATOM_CACHE, 16#40).
-define(DFLAG_NEW_FUN_TAGS, 16#80).
-define(DFLAG_EXTENDED_PIDS_PORTS, 16#100).
-define(DFLAG_EXPORT_PTR_TAG, 16#200).
-define(DFLAG_BIT_BINARIES, 16#400).
-define(DFLAG_NEW_FLOATS, 16#800).
-define(DFLAG_UNICODE_IO, 16#1000).
-define(DFLAG_DIST_HDR_ATOM_CACHE, 16#2000).
-define(DFLAG_SMALL_ATOM_TAGS, 16#4000).
-define(DFLAG_UTF8_ATOMS, 16#10000).
-define(DFLAG_MAP_TAG, 16#20000).
-define(DFLAG_BIG_CREATION, 16#40000).
-define(DFLAG_SEND_SENDER, 16#80000).
-define(DFLAG_BIG_SEQTRACE_LABELS, 16#100000).
% OTP-22
%% -define(DFLAG_NO_MAGIC, 16#200000). %% Used internally only
-define(DFLAG_EXIT_PAYLOAD, 16#400000).
-define(DFLAG_FRAGMENTS, 16#800000).
% OTP-23
-define(DFLAG_HANDSHAKE_23, 16#01000000).
-define(DFLAG_SPAWN, 16#100000000).
-define(DFLAG_NAME_ME, 16#200000000).
% OTP-24
-define(DFLAG_V4_NC, 16#400000000).
-define(DFLAG_ALIAS, 16#800000000).
% OTP-25
-define(DFLAG_UNLINK_ID, 16#02000000).
-define(DFLAG_MANDATORY_25_DIGEST, 16#04000000).

-define(MANDATORY_DFLAGS_25,
    (?DFLAG_EXTENDED_REFERENCES bor
        ?DFLAG_FUN_TAGS bor
        ?DFLAG_EXTENDED_PIDS_PORTS bor
        ?DFLAG_NEW_FUN_TAGS bor
        ?DFLAG_EXPORT_PTR_TAG bor
        ?DFLAG_BIT_BINARIES bor
        ?DFLAG_NEW_FLOATS bor
        ?DFLAG_UTF8_ATOMS bor
        ?DFLAG_MAP_TAG bor
        ?DFLAG_BIG_CREATION bor
        ?DFLAG_HANDSHAKE_23)
).

-define(MANDATORY_DFLAGS_26,
    (?DFLAG_V4_NC bor
        ?DFLAG_UNLINK_ID)
).

% Analysis of erts_internal:get_dflags() on OTP-27:
%                                    PREFERRED MANDATORY ALLOWED REJECTABLE
% ?DFLAG_PUBLISHED                       -         -        -       -
% ?DFLAG_ATOM_CACHE                      -         -        -       x
% ?DFLAG_EXTENDED_REFERENCES             x         x        x       -
% ?DFLAG_DIST_MONITOR                    x         -        x       -
% ?DFLAG_FUN_TAGS                        x         x        x       -
% ?DFLAG_DIST_MONITOR_NAME               x         -        x       -
% ?DFLAG_HIDDEN_ATOM_CACHE               -         -        -       x
% ?DFLAG_NEW_FUN_TAGS                    x         x        x       -
% ?DFLAG_EXTENDED_PIDS_PORTS             x         x        x       -
% ?DFLAG_EXPORT_PTR_TAG                  x         x        x       -
% ?DFLAG_BIT_BINARIES                    x         x        x       -
% ?DFLAG_NEW_FLOATS                      x         x        x       -
% ?DFLAG_UNICODE_IO                      x         -        x       -
% ?DFLAG_DIST_HDR_ATOM_CACHE             x         -        x       x
% ?DFLAG_SMALL_ATOM_TAGS                 x         -        x       -
% (reserved)
% ?DFLAG_UTF8_ATOMS                      x         x        x       -
% ?DFLAG_MAP_TAG                         x         x        x       -
% ?DFLAG_BIG_CREATION                    x         x        x       -
% ?DFLAG_SEND_SENDER                     x         -        x       -
% ?DFLAG_BIG_SEQTRACE_LABELS             x         -        x       -
% (?DFLAG_NO_MAGIC)
% ?DFLAG_EXIT_PAYLOAD                    x         -        x       -
% ?DFLAG_FRAGMENTS                       x         -        x       x
% ?DFLAG_HANDSHAKE_23                    x         x        x       -
% ?DFLAG_UNLINK_ID                       x         x        x       -
% (?DFLAG_MANDATORY_25_DIGEST)
% (reserved)
% ?DFLAG_SPAWN                           x         -        x       -
% (?DFLAG_NAME_ME)
% ?DFLAG_V4_NC                           x         x        x       -
% ?DFLAG_ALIAS                           x         -        x       -

% All preferred flags can be added by remote node.
% Many flags cannot be rejected, and are thus mandatory if connection is
% initiated by OTP-27.

-define(MANDATORY_DFLAGS,
    (?MANDATORY_DFLAGS_26 bor ?MANDATORY_DFLAGS_25 bor
        ?DFLAG_DIST_MONITOR bor
        ?DFLAG_DIST_MONITOR_NAME bor
        ?DFLAG_UNICODE_IO bor
        ?DFLAG_SMALL_ATOM_TAGS bor
        ?DFLAG_SEND_SENDER bor
        ?DFLAG_BIG_SEQTRACE_LABELS bor
        ?DFLAG_EXIT_PAYLOAD bor
        ?DFLAG_SPAWN bor
        ?DFLAG_ALIAS)
).

-define(UNSUPPORTED_DFLAGS, (?DFLAG_NAME_ME)).

-record(connection, {
    kernel :: pid(),
    node :: node(),
    socket :: any(),
    tick_intensity :: 4..1000,
    get_stat,
    send_tick,
    tick_timeout :: non_neg_integer(),
    last_read :: non_neg_integer(),
    last_written :: non_neg_integer(),
    dhandle :: reference()
}).

-spec net_ticker_spawn_options() -> [any()].
net_ticker_spawn_options() ->
    [link].

-spec start_timer(pos_integer()) -> pid().
start_timer(Timeout) ->
    Self = self(),
    spawn_link(fun() -> timer_loop(Self, Timeout) end).

timer_loop(Pid, Timeout) ->
    receive
        {Pid, reset} -> timer_loop(Pid, Timeout)
    after Timeout ->
        ?shutdown(timer_timeout)
    end.

-spec cancel_timer(pid()) -> ok.
cancel_timer(Timer) ->
    unlink(Timer),
    exit(Timer, cancel_timer),
    ok.

-spec reset_timer(pid()) -> ok.
reset_timer(Timer) ->
    Timer ! {self(), reset},
    ok.

-spec handshake_other_started(#hs_data{}) -> no_return().
handshake_other_started(#hs_data{socket = Socket, f_recv = Recv} = HSData0) ->
    case Recv(Socket, 0, infinity) of
        {ok, <<$N, Flags:64, Creation:32, NameLen:16, Rest/binary>>} ->
            {Name, _} = split_binary(Rest, NameLen),
            check_name(Name),
            NodeName = binary_to_atom(Name, latin1),
            HSData1 = HSData0#hs_data{
                other_node = NodeName,
                other_started = true,
                this_flags = ?MANDATORY_DFLAGS
            },
            check_flags(Flags, HSData1),
            mark_pending(HSData1),
            Cookie = net_kernel:get_cookie(HSData1#hs_data.other_node),
            <<Challenge:32>> = crypto:strong_rand_bytes(4),
            send_challenge(Challenge, HSData1),
            reset_timer(HSData1#hs_data.timer),
            {OtherChallenge, OtherDigest} = recv_challenge_reply(HSData1),
            check_challenge(Cookie, Challenge, OtherDigest, HSData1),
            send_challenge_ack(Cookie, OtherChallenge, HSData1),
            connection(HSData1, Creation);
        {ok, Other} ->
            ?shutdown({unexpected, Other});
        {error, Reason} ->
            ?shutdown2(recv_error, Reason)
    end.

-spec handshake_we_started(#hs_data{}) -> no_return().
handshake_we_started(#hs_data{} = HSData0) ->
    HSData1 = HSData0#hs_data{
        other_started = false,
        this_flags = ?MANDATORY_DFLAGS
    },
    send_name(HSData1),
    case recv_status(HSData1) of
        <<"ok">> -> ok;
        <<"ok_simultaneous">> -> ok;
        <<"nok">> -> ?shutdown({HSData1#hs_data.other_node, simultaneous});
        <<"alive">> -> send_status(<<"true">>, HSData1);
        Other -> ?shutdown({HSData1#hs_data.other_node, {unexpected, Other}})
    end,
    Cookie = net_kernel:get_cookie(HSData1#hs_data.other_node),
    {OtherChallenge, OtherFlags, Creation} = recv_challenge(HSData1),
    check_flags(OtherFlags, HSData1),
    <<MyChallenge:32>> = crypto:strong_rand_bytes(4),
    send_challenge_reply(Cookie, OtherChallenge, MyChallenge, HSData1),
    OtherDigest = recv_challenge_ack(HSData1),
    check_challenge(Cookie, MyChallenge, OtherDigest, HSData1),
    connection(HSData1, Creation).

% We are connected
-spec connection(#hs_data{}, non_neg_integer()) -> no_return().
connection(
    #hs_data{
        kernel_pid = Kernel,
        timer = Timer,
        f_setopts_pre_nodeup = PreNodeUp,
        f_setopts_post_nodeup = PostNodeUp,
        f_handshake_complete = HandshakeComplete,
        f_getll = GetLL,
        f_address = Address,
        mf_getstat = GetStat,
        mf_tick = SendTick,
        socket = Socket,
        other_node = Node,
        this_flags = Flags
    } = HSData,
    Creation
) ->
    cancel_timer(Timer),
    case PreNodeUp(Socket) of
        ok -> ok;
        Error1 -> ?shutdown2({Node, Socket}, Error1)
    end,
    LL =
        case GetLL(Socket) of
            {ok, LL0} -> LL0;
            Error2 -> ?shutdown2({Node, Socket}, Error2)
        end,
    % We don't negotiate flags, other flags are our flags
    DHandle = erlang:setnode(Node, LL, {Flags, Creation}),
    AddressRecord = Address(Socket, Node),
    TickIntensity = mark_nodeup(AddressRecord, HSData),
    case PostNodeUp(Socket) of
        ok -> ok;
        Error3 -> ?shutdown2({Node, Socket}, Error3)
    end,
    case HandshakeComplete of
        undefined -> ok;
        _ -> HandshakeComplete(Socket, Node, DHandle)
    end,
    Connection = #connection{
        kernel = Kernel,
        node = Node,
        socket = Socket,
        tick_intensity = TickIntensity,
        get_stat = GetStat,
        send_tick = SendTick,
        tick_timeout = TickIntensity - 1,
        last_read = 0,
        last_written = 0,
        dhandle = DHandle
    },
    connection_loop(Connection).

connection_loop(#connection{kernel = Kernel} = Connection) ->
    receive
        {Kernel, disconnect} ->
            ?shutdown2({Connection#connection.node, Connection#connection.socket}, disconnected);
        {Kernel, tick} ->
            LastRead = Connection#connection.last_read,
            LastWritten = Connection#connection.last_written,
            TickTimeout = Connection#connection.tick_timeout,
            Socket = Connection#connection.socket,
            GetStat = Connection#connection.get_stat,
            case GetStat(Socket) of
                {ok, LastRead, _Write, _Pending} when TickTimeout =:= 0 ->
                    ?shutdown2(
                        {Connection#connection.node, Connection#connection.socket}, net_tick_timeout
                    );
                {ok, LastRead, LastWritten, 0} ->
                    % nothing read, nothing written and send queue is empty: send tick
                    SendTick = Connection#connection.send_tick,
                    SendTick(Socket),
                    connection_loop(Connection#connection{
                        last_written = LastWritten + 1, tick_timeout = TickTimeout - 1
                    });
                {ok, NewRead, LastWritten, _} ->
                    % nothing read and send queue is empty: send tick
                    SendTick = Connection#connection.send_tick,
                    SendTick(Socket),
                    connection_loop(Connection#connection{
                        last_written = LastWritten + 1,
                        last_read = NewRead,
                        tick_timeout = Connection#connection.tick_intensity - 1
                    });
                {ok, NewRead, NewWrite, _} ->
                    connection_loop(Connection#connection{
                        last_read = NewRead,
                        last_written = NewWrite,
                        tick_timeout = Connection#connection.tick_intensity - 1
                    });
                {error, Reason} ->
                    ?shutdown2({Connection#connection.node, Connection#connection.socket}, Reason)
            end;
        Other ->
            io:format("Unexpected message in conn_loop : ~p\n", [Other])
    end.

-spec check_flags(non_neg_integer(), #hs_data{}) -> ok.
check_flags(Flags0, HSData) ->
    Flags1 =
        if
            Flags0 band ?DFLAG_MANDATORY_25_DIGEST ->
                Flags0 bor ?MANDATORY_DFLAGS_25;
            true ->
                Flags0
        end,
    CheckResult =
        if
            Flags1 band ?MANDATORY_DFLAGS =/= ?MANDATORY_DFLAGS ->
                {error, {missing_flags, ?MANDATORY_DFLAGS bxor (Flags1 band ?MANDATORY_DFLAGS)}};
            Flags1 band ?UNSUPPORTED_DFLAGS =/= 0 ->
                {error, {unsupported_flags, Flags1 band ?UNSUPPORTED_DFLAGS}};
            true ->
                ok
        end,
    case CheckResult of
        ok ->
            ok;
        {error, Reason} when HSData#hs_data.other_started ->
            % send_status to say we don't accept this
            send_status(<<"not_allowed">>, HSData),
            ?shutdown(Reason);
        {error, Reason} ->
            ?shutdown(Reason)
    end.

% send name
send_name(
    #hs_data{socket = Socket, f_send = Send, this_node = ThisNode, this_flags = ThisFlags} = HSData
) ->
    Creation = atomvm:get_creation(),
    NodeName = atom_to_binary(ThisNode, latin1),
    NameLen = byte_size(NodeName),
    case Send(Socket, <<$N, ThisFlags:64, Creation:32, NameLen:16, NodeName/binary>>) of
        {error, _} = Error ->
            ?shutdown2({HSData#hs_data.other_node, Socket}, {send_name_failed, Error});
        ok ->
            ok
    end.

% Ensure name is somewhat valid
-spec check_name(binary()) -> ok.
check_name(Name) ->
    case binary:split(Name, <<"@">>, [global]) of
        [<<_, _NamePartRest/binary>>, <<_, _HostPartRest/binary>>] ->
            ok;
        _Other ->
            ?shutdown({unsupported_name, Name})
    end.

-spec send_status(binary(), #hs_data{}) -> ok.
send_status(Status, #hs_data{socket = Socket, f_send = Send} = HSData) ->
    case Send(Socket, <<$s, Status/binary>>) of
        {error, _} = Error ->
            ?shutdown2({HSData#hs_data.other_node, Socket}, {send_status_failed, Error});
        ok ->
            ok
    end.

-spec recv_status(#hs_data{}) -> binary().
recv_status(#hs_data{socket = Socket, f_recv = Recv} = HSData) ->
    case Recv(Socket, 0, infinity) of
        {ok, <<$s, Result/binary>>} ->
            Result;
        {ok, Other} ->
            ?shutdown({HSData#hs_data.other_node, {unexpected, recv_status, Other}});
        {error, Reason} ->
            ?shutdown2({HSData#hs_data.other_node, recv_error}, Reason)
    end.

-spec mark_pending(#hs_data{}) -> ok.
mark_pending(#hs_data{kernel_pid = Kernel, this_node = ThisNode, other_node = OtherNode} = HSData) ->
    case net_kernel:mark_pending(Kernel, ThisNode, OtherNode, self()) of
        ok ->
            send_status(<<"ok">>, HSData);
        {ok_simultaneous, OtherConnPid} ->
            send_status(<<"ok_simultaneous">>, HSData),
            exit(OtherConnPid, shutdown);
        nok ->
            send_status(<<"nok">>, HSData),
            ?shutdown(OtherNode);
        alive ->
            send_status(<<"alive">>, HSData),
            reset_timer(HSData#hs_data.timer),
            case recv_status(HSData) of
                <<"true">> -> ok;
                <<"false">> -> ?shutdown(OtherNode);
                Other -> ?shutdown({OtherNode, {unexpected, Other}})
            end
    end.

mark_nodeup(Address, #hs_data{kernel_pid = Kernel, other_node = Node}) ->
    case net_kernel:mark_nodeup(Kernel, Node, Address, self()) of
        {ok, TickIntensity} ->
            TickIntensity;
        {error, Reason} ->
            ?shutdown2({mark_nodeup, Node, Address}, Reason)
    end.

send_challenge(
    Challenge,
    #hs_data{this_node = ThisNode, this_flags = ThisFlags, socket = Socket, f_send = Send} = HSData
) ->
    Creation = atomvm:get_creation(),
    NodeName = atom_to_binary(ThisNode, latin1),
    NameLen = byte_size(NodeName),
    case
        Send(Socket, <<$N, ThisFlags:64, Challenge:32, Creation:32, NameLen:16, NodeName/binary>>)
    of
        {error, _} = Error ->
            ?shutdown2({HSData#hs_data.other_node, Socket}, {send_challenge_failed, Error});
        ok ->
            ok
    end.

recv_challenge(
    #hs_data{other_node = OtherNode, socket = Socket, f_recv = Recv} = HSData
) ->
    case Recv(Socket, 0, infinity) of
        {ok, <<
            $N, OtherFlags:64, Challenge:32, OtherCreation:32, _OtherNameLen:16, OtherName/binary
        >>} ->
            case atom_to_binary(OtherNode, utf8) =/= OtherName of
                true ->
                    ?shutdown({
                        HSData#hs_data.other_node, {mismatch, recv_challenge, OtherNode, OtherName}
                    });
                false ->
                    ok
            end,
            {Challenge, OtherFlags, OtherCreation};
        {ok, Other} ->
            ?shutdown({HSData#hs_data.other_node, {unexpected, recv_challenge, Other}});
        {error, Reason} ->
            ?shutdown2({HSData#hs_data.other_node, recv_error}, Reason)
    end.

-spec recv_challenge_reply(#hs_data{}) -> {non_neg_integer(), binary()}.
recv_challenge_reply(#hs_data{socket = Socket, f_recv = Recv} = HSData) ->
    case Recv(Socket, 0, infinity) of
        {ok, <<$r, OtherChallenge:32, OtherDigest:16/binary>>} ->
            {OtherChallenge, OtherDigest};
        {ok, Other} ->
            ?shutdown({HSData#hs_data.other_node, {unexpected, recv_challenge_reply, Other}});
        {error, Reason} ->
            ?shutdown2({HSData#hs_data.other_node, recv_error}, Reason)
    end.

-spec send_challenge_reply(
    Cookie :: binary(),
    OtherChallenge :: non_neg_integer(),
    MyChallenge :: non_neg_integer(),
    #hs_data{}
) -> ok.
send_challenge_reply(
    Cookie, OtherChallenge, MyChallenge, #hs_data{socket = Socket, f_send = Send} = HSData
) ->
    Digest = gen_digest(Cookie, OtherChallenge),
    case Send(Socket, <<$r, MyChallenge:32, Digest:16/binary>>) of
        {error, _} = Error ->
            ?shutdown2({HSData#hs_data.other_node, Socket}, {send_challenge_reply_failed, Error});
        ok ->
            ok
    end.

-spec check_challenge(
    Cookie :: binary(), Challenge :: non_neg_integer(), Digest :: binary(), #hs_data{}
) -> ok.
check_challenge(Cookie, Challenge, Digest, HSData) ->
    case gen_digest(Cookie, Challenge) =:= Digest of
        true ->
            ok;
        false ->
            ?shutdown({HSData#hs_data.other_node, invalid_challenge})
    end.

-spec gen_digest(Cookie :: binary(), Challenge :: non_neg_integer()) -> binary().
gen_digest(Cookie, Challenge) ->
    crypto:hash(md5, [Cookie, integer_to_list(Challenge)]).

-spec send_challenge_ack(Cookie :: binary(), Challenge :: non_neg_integer(), #hs_data{}) -> ok.
send_challenge_ack(Cookie, Challenge, #hs_data{socket = Socket, f_send = Send} = HSData) ->
    Digest = gen_digest(Cookie, Challenge),
    case Send(Socket, <<$a, Digest/binary>>) of
        {error, _} = Error ->
            ?shutdown2({HSData#hs_data.other_node, Socket}, {send_challenge_failed, Error});
        ok ->
            ok
    end.

-spec recv_challenge_ack(#hs_data{}) -> binary().
recv_challenge_ack(#hs_data{socket = Socket, f_recv = Recv} = HSData) ->
    case Recv(Socket, 0, infinity) of
        {ok, <<$a, Digest/binary>>} ->
            Digest;
        {ok, Other} ->
            ?shutdown({HSData#hs_data.other_node, {unexpected, recv_challenge_ack, Other}});
        {error, _} = Error ->
            ?shutdown2({HSData#hs_data.other_node, Socket}, {recv_challenge_ack, Error})
    end.

-spec shutdown(atom(), non_neg_integer(), term()) -> no_return().
shutdown(Module, Line, Data) ->
    shutdown(Module, Line, Data, shutdown).

-spec shutdown(atom(), non_neg_integer(), term(), term()) -> no_return().
shutdown(Module, Line, Data, Reason) ->
    io:format("~s: shutting down connection ~p:~p, data=~p, reason=~p\n", [
        ?MODULE, Module, Line, Data, Reason
    ]),
    exit(Reason).
