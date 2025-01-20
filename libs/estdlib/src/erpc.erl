%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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
%% @doc An implementation of the Erlang/OTP erpc interface.
%%
%% This module implements a strict subset of the Erlang/OTP erpc
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(erpc).

% api
-export([
    execute_call/4
]).

%%-----------------------------------------------------------------------------
%% @param Reference reference of the request, passed in exit tuple
%% @param Module module to call
%% @param Func function to call
%% @param Args argument of the call
%% @doc Execute a call locally, exiting with the result.
%% This function is called from rpc on other nodes using spawn_request BIF.
%% @end
%%-----------------------------------------------------------------------------
-spec execute_call(Reference :: reference(), Module :: module(), Func :: atom(), Args :: [any()]) ->
    no_return().
execute_call(Reference, Module, Func, Args) ->
    Reply =
        try
            Result = apply(Module, Func, Args),
            {Reference, return, Result}
        catch
            throw:Reason ->
                {Reference, throw, Reason};
            exit:Reason ->
                {Reference, exit, Reason};
            error:Reason:Stack ->
                {Reference, error, Reason, Stack}
        end,
    exit(Reply).
