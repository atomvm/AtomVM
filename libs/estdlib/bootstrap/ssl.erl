%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(ssl).

-export([
    start/0,
    stop/0,
    connect/3,
    close/1,
    send/2,
    recv/2
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-export([
    nif_close_notify/1,
    nif_conf_authmode/2,
    nif_conf_rng/2,
    nif_config_defaults/3,
    nif_config_init/0,
    nif_ctr_drbg_init/0,
    nif_ctr_drbg_seed/3,
    nif_entropy_init/0,
    nif_handshake_step/1,
    nif_init/0,
    nif_read/2,
    nif_set_bio/2,
    nif_set_hostname/2,
    nif_setup/2,
    nif_write/2
]).

% Resources
-type entropy() :: binary().
-type ctrdrbg() :: binary().
-type sslcontext() :: binary().
-type sslconfig() :: binary().

-opaque sslsocket() :: {sslcontext(), socket:socket()}.

-export_type([
    sslsocket/0,
    host/0,
    hostname/0
]).

-type host() :: hostname() | ip_address().
-type hostname() :: string().
-type ip_address() :: inet:ip_address().
-type tls_client_option() :: client_option().
-type client_option() ::
    {server_name_indication, sni()}.
-type sni() :: hostname() | disabled.
-type reason() :: any().

-spec start() -> ok.
start() ->
    try
        {ok, _Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], [])
    catch
        error:{badmatch, {error, {already_started, _}}} ->
            ok
    end,
    ok.

-spec stop() -> ok.
stop() ->
    ok = gen_server:call(?MODULE, stop).

-record(state, {
    ctr_drbg :: ctrdrbg(),
    entropy :: entropy()
}).

init([]) ->
    Entropy = ?MODULE:nif_entropy_init(),
    CtrDrbg = ?MODULE:nif_ctr_drbg_init(),
    ok = ?MODULE:nif_ctr_drbg_seed(CtrDrbg, Entropy, <<"AtomVM">>),
    {ok, #state{entropy = Entropy, ctr_drbg = CtrDrbg}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(get_ctr_drbg, _From, #state{ctr_drbg = CtrDrbg} = State) ->
    {reply, CtrDrbg, State};
handle_call(get_entropy, _From, #state{entropy = Entropy} = State) ->
    {reply, Entropy, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

-spec connect(Host :: host(), Port :: inet:port_number(), TLSOptions :: [tls_client_option()]) ->
    {ok, sslsocket()} | {error, reason()}.
connect(Hostname, Port, TLSOptions) when
    is_list(Hostname) andalso is_integer(Port) andalso is_list(TLSOptions)
->
    % Erlang OTP actually first checks some options
    case net:getaddrinfo(Hostname) of
        {ok, Results} ->
            case
                [
                    Addr
                 || #{addr := #{addr := Addr}, type := stream, protocol := tcp, family := inet} <-
                        Results
                ]
            of
                [TCPAddr | _] ->
                    NewTLSOptions =
                        case lists:keyfind(server_name_indication, 1, TLSOptions) of
                            false -> [{server_name_indication, Hostname} | TLSOptions];
                            _ -> TLSOptions
                        end,
                    connect(TCPAddr, Port, NewTLSOptions);
                [] ->
                    {error, nxdomain}
            end;
        {error, _} ->
            {error, nxdomain}
    end;
connect(Addr, Port, TLSOptions) when
    is_tuple(Addr) andalso is_integer(Port) andalso is_list(TLSOptions)
->
    {ok, Socket} = socket:open(inet, stream, tcp),
    case socket:connect(Socket, #{family => inet, addr => Addr, port => Port}) of
        ok ->
            connect(Socket, TLSOptions);
        {error, _Reason} ->
            {error, _Reason}
    end.

-spec connect(Socket :: socket:socket(), TLSOptions :: [tls_client_option()]) ->
    {ok, sslsocket()} | {error, reason()}.
connect(Socket, TLSOptions) ->
    SSLContext = ?MODULE:nif_init(),
    ok = ?MODULE:nif_set_bio(SSLContext, Socket),
    SSLConfig = ?MODULE:nif_config_init(),
    ok = ?MODULE:nif_config_defaults(SSLConfig, client, stream),
    process_options(SSLContext, SSLConfig, TLSOptions),
    CtrDrbg = gen_server:call(?MODULE, get_ctr_drbg),
    ok = ?MODULE:nif_conf_rng(SSLConfig, CtrDrbg),
    ok = ?MODULE:nif_setup(SSLContext, SSLConfig),
    handshake_loop(SSLContext, Socket).

handshake_loop(SSLContext, Socket) ->
    case ?MODULE:nif_handshake_step(SSLContext) of
        ok ->
            handshake_loop(SSLContext, Socket);
        done ->
            {ok, {SSLContext, Socket}};
        want_read ->
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {select, _SocketResource, Ref, ready_input} ->
                            handshake_loop(SSLContext, Socket);
                        {closed, Ref} ->
                            ok = socket:close(Socket),
                            {error, closed}
                    end;
                {error, _Reason} = Error ->
                    socket:close(Socket),
                    Error
            end;
        want_write ->
            % We're currrently missing non-blocking writes
            handshake_loop(SSLContext, Socket);
        {error, _Reason} = Error ->
            socket:close(Socket),
            Error
    end.

-spec process_options(
    SSLContext :: sslcontext(), SSLConfig :: sslconfig(), TLSOptions :: [tls_client_option()]
) -> ok.
process_options(_SSLContext, _SSLConfig, []) ->
    ok;
process_options(SSLContext, SSLConfig, [{server_name_indication, disabled} | Tail]) ->
    process_options(SSLContext, SSLConfig, Tail);
process_options(SSLContext, SSLConfig, [{server_name_indication, Hostname} | Tail]) ->
    ok = ?MODULE:nif_set_hostname(SSLContext, Hostname),
    process_options(SSLContext, SSLConfig, Tail);
process_options(SSLContext, SSLConfig, [{verify, verify_none} | Tail]) ->
    ok = ?MODULE:nif_conf_authmode(SSLConfig, none),
    process_options(SSLContext, SSLConfig, Tail);
process_options(SSLContext, SSLConfig, [{binary, true} | Tail]) ->
    process_options(SSLContext, SSLConfig, Tail);
process_options(SSLContext, SSLConfig, [{active, false} | Tail]) ->
    process_options(SSLContext, SSLConfig, Tail).

-spec close(sslsocket()) -> ok.
close({SSLContext, Socket}) ->
    _ = close_notify_loop(SSLContext, Socket),
    ok = socket:close(Socket),
    ok.

close_notify_loop(SSLContext, Socket) ->
    case ?MODULE:nif_close_notify(SSLContext) of
        ok ->
            ok;
        want_read ->
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {select, _SocketResource, Ref, ready_input} ->
                            close_notify_loop(SSLContext, Socket);
                        {closed, Ref} ->
                            ok = socket:close(Socket),
                            {error, closed}
                    end;
                {error, _Reason} = Error ->
                    socket:close(Socket),
                    Error
            end;
        want_write ->
            % We're currrently missing non-blocking writes
            close_notify_loop(SSLContext, Socket);
        {error, _Reason} = Error ->
            socket:close(Socket),
            Error
    end.

-spec send(Socket :: sslsocket(), Data :: iodata()) -> ok | {error, reason()}.
send(SSLSocket, IOList) when is_list(IOList) ->
    send(SSLSocket, iolist_to_binary(IOList));
send({SSLContext, Socket} = SSLSocket, Binary) ->
    case ?MODULE:nif_write(SSLContext, Binary) of
        ok ->
            ok;
        {ok, Rest} ->
            send(SSLSocket, Rest);
        want_read ->
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {select, _SocketResource, Ref, ready_input} ->
                            send(SSLSocket, Binary);
                        {closed, Ref} ->
                            {error, closed}
                    end;
                {error, _Reason} = Error ->
                    Error
            end;
        want_write ->
            % We're currrently missing non-blocking writes
            send(SSLSocket, Binary);
        {error, _Reason} = Error ->
            Error
    end.

-spec recv(Socket :: sslsocket(), Length :: non_neg_integer()) -> ok | {error, reason()}.
recv(SSLSocket, Length) ->
    recv0(SSLSocket, Length, []).

recv0(_SSLSocket, 0, Acc) ->
    {ok, list_to_binary(lists:reverse(Acc))};
recv0({SSLContext, Socket} = SSLSocket, Remaining, Acc) ->
    case ?MODULE:nif_read(SSLContext, Remaining) of
        {ok, Data} ->
            Len = byte_size(Data),
            recv0(SSLSocket, Remaining - Len, [Data | Acc]);
        want_read ->
            Ref = erlang:make_ref(),
            case socket:nif_select_read(Socket, Ref) of
                ok ->
                    receive
                        {select, _SocketResource, Ref, ready_input} ->
                            recv0(SSLSocket, Remaining, Acc);
                        {closed, Ref} ->
                            {error, closed}
                    end;
                {error, _Reason} = Error ->
                    Error
            end;
        want_write ->
            % We're currrently missing non-blocking writes
            recv0(SSLSocket, Remaining, Acc);
        {error, _Reason} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% NIF Functions
%%-----------------------------------------------------------------------------

%% @private
-spec nif_entropy_init() -> entropy().
nif_entropy_init() ->
    erlang:nif_error(undefined).

%% @private
-spec nif_ctr_drbg_init() -> ctrdrbg().
nif_ctr_drbg_init() ->
    erlang:nif_error(undefined).

%% @private
-spec nif_ctr_drbg_seed(CtrDrbg :: ctrdrbg(), Entropy :: entropy(), Custom :: binary()) -> ok.
nif_ctr_drbg_seed(_CtrDrbg, _Entropy, _Custom) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_init() -> sslcontext().
nif_init() ->
    erlang:nif_error(undefined).

%% @private
-spec nif_config_init() -> sslconfig().
nif_config_init() ->
    erlang:nif_error(undefined).

%% @private
-spec nif_config_defaults(
    Config :: sslconfig(), Endpoint :: client | server, Transport :: stream | dgram
) -> ok.
nif_config_defaults(_Config, _Endpoint, _Transport) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_conf_authmode(Config :: sslconfig(), none | optional | required) -> ok.
nif_conf_authmode(_Config, _AuthMode) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_conf_rng(Config :: sslconfig(), CtrDrbg :: ctrdrbg()) -> ok.
nif_conf_rng(_Config, _CtrDrbg) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_setup(Context :: sslcontext(), Config :: sslconfig()) -> ok.
nif_setup(_Context, _Config) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_set_bio(Context :: sslcontext(), Socket :: socket:socket()) -> ok.
nif_set_bio(_Context, _Socket) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_set_hostname(Context :: sslcontext(), Hostname :: hostname()) -> ok.
nif_set_hostname(_Context, _Hostname) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_handshake_step(Context :: sslcontext()) ->
    ok | done | want_read | want_write | {error, reason()}.
nif_handshake_step(_Context) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_close_notify(Context :: sslcontext()) -> ok | want_read | want_write | {error, reason()}.
nif_close_notify(_Context) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_read(Context :: sslcontext(), Length :: non_neg_integer()) ->
    {ok, binary()} | want_read | want_write | {error, reason()}.
nif_read(_Context, _Length) ->
    erlang:nif_error(undefined).

%% @private
-spec nif_write(Context :: sslcontext(), Len :: non_neg_integer()) ->
    {ok, binary()} | want_read | want_write | {error, reason()}.
nif_write(_Context, _Binary) ->
    erlang:nif_error(undefined).
