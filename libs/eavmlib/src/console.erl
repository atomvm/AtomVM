%
% This file is part of AtomVM.
%
% Copyright 2018-2022 Fred Dushin <fred@dushin.net>
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

%
% This file is part of AtomVM.
%
% Copyright 2018 Fred Dushin <fred@dushin.net>
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
%% @doc This modules supports output of string data to the console.
%% @end
%%-----------------------------------------------------------------------------
-module(console).

-export([start/0, puts/1, puts/2, flush/0, flush/1, print/1]).

%%-----------------------------------------------------------------------------
%% @param   String the string data to write to the console
%% @returns ok if the data was written, or {error, Reason}, if there was
%%          an error.
%% @see     erlang:display/1
%% @doc     Write a string to the console.
%%
%%          <em><b>Note.</b>  This operation will only write string data.
%%          The output is not suffixed with a newline character or sequence.
%%          To print an erlang term, use erlang:display/1.</em>
%% @end
%%-----------------------------------------------------------------------------
-spec puts(string()) -> ok | {error, term()}.
puts(String) ->
    puts(get_pid(), String).

%% @hidden
-spec puts(pid(), string()) -> ok.
puts(Console, String) ->
    call(Console, {puts, String}).

%%-----------------------------------------------------------------------------
%% @returns ok if the data was written, or {error, Reason}, if there was
%%          an error.
%% @doc     Flush any previously written data to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec flush() -> ok.
flush() ->
    flush(get_pid()).

%% @hidden
-spec flush(pid()) -> ok.
flush(Console) ->
    call(Console, flush).

%%-----------------------------------------------------------------------------
%% @param   String the string data to write to the console
%% @returns ok if the data was written, or {error, Reason}, if there was
%%          an error.
%% @see     erlang:display/1
%% @doc     Write a string to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec print(string()) -> ok | error.
print(String) ->
    throw(nif_error).

%% Internal operations

%% @private
-spec call(pid(), string()) -> ok.
call(Console, Msg) ->
    Ref = make_ref(),
    Console ! {self(), Ref, Msg},
    receive
        {Ref, Reply} -> Reply
    end.

%% @private
-spec get_pid() -> pid().
get_pid() ->
    case whereis(console) of
        undefined ->
            start();
        Pid when is_pid(Pid) ->
            Pid
    end.

%% @private
-spec start() -> pid().
start() ->
    Pid = erlang:open_port({spawn, "console"}, []),
    erlang:register(console, Pid),
    Pid.
