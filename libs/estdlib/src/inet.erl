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

-module(inet).

-export([port/1, close/1, sockname/1, peername/1, getaddr/2]).

-include("inet-priv.hrl").

-type moniker() :: ?GEN_TCP_MONIKER | ?GEN_UDP_MONIKER.
-type socket_impl() :: any().
-type socket() :: {moniker(), socket_impl(), module()}.
-type port_number() :: 0..65535.
-type ip_address() :: ip4_address().
-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
-type hostname() :: atom() | string().
-type address_family() :: inet.

-export_type([socket/0, port_number/0, ip_address/0, ip4_address/0, hostname/0]).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket from which to obtain the port number
%% @returns the port number associated with the local socket
%% @doc     Retrieve the actual port number to which the socket is bound.
%%          This function is useful if the port assignment is done by the
%%          operating system.
%% @end
%%-----------------------------------------------------------------------------
-spec port(Socket :: socket()) -> port_number().
port({Moniker, Socket, Module}) when
    Moniker =:= ?GEN_TCP_MONIKER orelse Moniker =:= ?GEN_UDP_MONIKER
->
    Module:port(Socket).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket to close
%% @returns ok.
%% @doc     Close the socket.
%% @end
%%-----------------------------------------------------------------------------
-spec close(Socket :: socket()) -> ok.
close({Moniker, Socket, Module}) when
    Moniker =:= ?GEN_TCP_MONIKER orelse Moniker =:= ?GEN_UDP_MONIKER
->
    Module:close(Socket).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns The address and port of the local end of an established connection.
%% @doc     The address and port representing the "local" end of a connection.
%%          This function should be called on a running socket instance.
%% @end
%%-----------------------------------------------------------------------------
-spec sockname(Socket :: socket()) ->
    {ok, {ip_address(), port_number()}} | {error, Reason :: term()}.
sockname({Moniker, Socket, Module}) when
    Moniker =:= ?GEN_TCP_MONIKER orelse Moniker =:= ?GEN_UDP_MONIKER
->
    Module:sockname(Socket).

%%-----------------------------------------------------------------------------
%% @param   Socket the socket
%% @returns The address and port of the remote end of an established connection.
%% @doc     The address and port representing the "remote" end of a connection.
%%          This function should be called on a running socket instance.
%% @end
%%-----------------------------------------------------------------------------
-spec peername(Socket :: socket()) ->
    {ok, {ip_address(), port_number()}} | {error, Reason :: term()}.
peername({?GEN_TCP_MONIKER, Socket, Module}) ->
    Module:peername(Socket).

%%-----------------------------------------------------------------------------
%% @param   Name the name to resolve
%% @param   Family the family to resolve it to
%% @returns The address or an error tuple.
%% @doc     Get the IP address associated with a given name.
%% @end
%%-----------------------------------------------------------------------------
-spec getaddr(Name :: ip_address() | hostname(), Family :: address_family()) ->
    {ok, ip_address()} | {error, Reason :: term()}.
getaddr({A, B, C, D} = Name, _Family) when
    is_integer(A) andalso A >= 0 andalso A < 256 andalso
        is_integer(B) andalso B >= 0 andalso B < 256 andalso
        is_integer(C) andalso C >= 0 andalso C < 256 andalso
        is_integer(D) andalso D >= 0 andalso D < 256
->
    {ok, Name};
getaddr(Name, Family) when is_atom(Name) ->
    getaddr(atom_to_list(Name), Family);
getaddr(Name, Family) when is_list(Name) ->
    try net:getaddrinfo(Name) of
        {ok, Results} ->
            Filtered = [Addr || #{family := F, addr := #{addr := Addr}} <- Results, F =:= Family],
            case Filtered of
                [] -> {error, nxdomain};
                [IPAddr | _] -> {ok, IPAddr}
            end;
        {error, eainoname} ->
            case string:split(Name, ".") of
                [Name] ->
                    % BEAM succeeds to resolve short names even if gethostbyname(2) fails.
                    % Work around for distribution by trying to add .local suffix.
                    case net:getaddrinfo(Name ++ ".local") of
                        {ok, ResultsLocal} ->
                            FilteredLocal = [
                                Addr
                             || #{family := F, addr := #{addr := Addr}} <- ResultsLocal,
                                F =:= Family,
                                Addr =:= {127, 0, 0, 1} orelse Addr =:= {0, 0, 0, 0, 0, 0, 0, 1}
                            ],
                            case FilteredLocal of
                                [] -> {error, nxdomain};
                                [LocalIPAddr | _] -> {ok, LocalIPAddr}
                            end;
                        _ ->
                            {error, nxdomain}
                    end;
                _ ->
                    {error, nxdomain}
            end;
        {error, -5} ->
            {error, nxdomain};
        {error, _} = Err ->
            Err
    catch
        error:function_clause ->
            {error, einval}
    end;
getaddr(_Name, _Family) ->
    {error, einval}.
