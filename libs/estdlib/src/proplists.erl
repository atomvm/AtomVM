%
% This file is part of AtomVM.
%
% Copyright 2019 Fred Dushin <fred@dushin.net>
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
%% @doc An implementation of the Erlang/OTP proplists interface.
%%
%% This module implements a strict susbset of the Erlang/OTP proplists
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(proplists).

-export([get_value/2, get_value/3]).

-type property() :: atom() | {term(), term()}.

%%-----------------------------------------------------------------------------
%% @equiv   get_value(Key, List, undefined)
%% @doc     Get a value from a property list.
%% @end
%%-----------------------------------------------------------------------------
-spec get_value(Key::term(), List::list(property())) -> term() | true | undefined.
get_value(Key, List) ->
    get_value(Key, List, undefined).

%%-----------------------------------------------------------------------------
%% @param   Key the key with which to find the value
%% @param   List the property list from which to get the value
%% @param   Default the default value to return, if Key is not in the property list.
%% @returns the value in the property list under the key, or Default, if Key is
%%          not in List.
%% @doc     Get a value from a property list.
%%
%%          Returns the value under the specified key, or the specified Default,
%%          if the Key is not in the supplied List.  If the Key corresponds to
%%          an entry in the property list that is just a single atom, this
%%          function returns the atom true.
%% @end
%%-----------------------------------------------------------------------------
-spec get_value(Key::term(), list(property()), Default::term()) -> term().
get_value(_Key, [], Default) ->
    Default;
get_value(Key, [{Key, Value} | _T], _Default) ->
    Value;
get_value(Key, [Key | _T], _Default) when is_atom(Key) ->
    true;
get_value(Key, [_H | T], Default) ->
    get_value(Key, T, Default).
