%
% This file is part of AtomVM.
%
% Copyright 2023 Fred Dushin <fred@dushin.net>
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

-module(net).

-export([getaddrinfo/1, getaddrinfo/2]).

%% nif call (so we can use guards at the API)
-export([getaddrinfo_nif/2]).

-type addrinfo() :: #{
    family := socket:domain(),
    socktype := socket:type(),
    protocol := socket:protocol(),
    %% NB. The erlang docs appear to be wrong about this
    address := socket:sockaddr(),
    addr := socket:sockaddr()
}.

-type service() :: string().

-export_type([addrinfo/0, service/0]).

%%-----------------------------------------------------------------------------
%% @param   Host the host string for which to find address information
%% @returns Address info for the specified host
%% @equiv   getaddrinfo(Host, undefined)
%% @doc     Retrieve address information for a given hostname.
%% @end
%%-----------------------------------------------------------------------------
-spec getaddrinfo(Host :: string()) -> {ok, AddrInfo :: addrinfo()} | {error, Reason :: term()}.
getaddrinfo(Host) when is_list(Host) ->
    ?MODULE:getaddrinfo(Host, undefined).

%%-----------------------------------------------------------------------------
%% @param   Host the host string for which to find address information
%% @param   Service the service string for which to find address information
%% @returns Address info for the specified host and service
%% @doc     Retrieve address information for a given hostname and service.
%%
%%          The `Host' parameter may be a fully qualified host name or a string
%%          containing a valid dotted pair IP address.  (Currently, only IPv4 is
%%          supported).
%%
%%          The `Service' parameter may be the name of a service (as defined via
%%          `services(3)` or a string containing a decimal value of the same.
%%
%%          Note that the `Host' or `String' parameter may be `undefined', but
%%          not both.
%% @end
%%-----------------------------------------------------------------------------
-spec getaddrinfo(Host :: string() | undefined, Service :: service() | undefined) ->
    {ok, AddrInfo :: addrinfo()} | {error, Reason :: term()}.
getaddrinfo(Host, Service) when
    (is_list(Host) orelse Host =:= undefined) andalso
        (is_list(Service) orelse Service =:= undefined) andalso
        not (Host =:= undefined andalso Service =:= undefined)
->
    ?MODULE:getaddrinfo_nif(Host, Service).

%% @hidden
getaddrinfo_nif(_Host, _Service) ->
    erlang:nif_error(undefined).
