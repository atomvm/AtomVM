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
%% @doc An implementation of the Erlang/OTP erlang module, for functions
%% that are not already defined as NIFs.
%% @end
%%-----------------------------------------------------------------------------
-module(erlang).

-export([
    apply/3,
    start_timer/3, start_timer/4,
    cancel_timer/1,
    send_after/3,
    process_info/2,
    system_info/1,
    system_flag/2,
    md5/1,
    is_map/1,
    map_size/1,
    map_get/2,
    map_is_key/2,
    monotonic_time/1,
    min/2,
    max/2,
    memory/1
]).

%%
%% TODO Correct the following bugs
%% * cancel_timer should be renamed cancel, per the OTP documentation
%% * return value needs to be {ok, cancel} or {error, Reason}
%% * review API documentation for timer functions in this module
%%

-type mem_type() :: binary().
-type time_unit() :: second | millisecond | microsecond.

%%-----------------------------------------------------------------------------
%% @param   Time time in milliseconds after which to send the timeout message.
%% @param   Dest Pid or server name to which to send the timeout message.
%% @param   Msg Message to send to Dest after Time ms.
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Start a timer, and send {timeout, TimerRef, Msg} to Dest after
%%          Time ms, where TimerRef is the reference returned from this function.
%% @end
%%-----------------------------------------------------------------------------
-spec start_timer(Time :: non_neg_integer(), Dest :: pid() | atom(), Msg :: term()) -> reference().
start_timer(Time, Dest, Msg) ->
    start_timer(Time, Dest, Msg, []).

%%-----------------------------------------------------------------------------
%% @hidden
%% @param   Time time in milliseconds after which to send the timeout message.
%% @param   Dest Pid or server name to which to send the timeout message.
%% @param   Msg Message to send to Dest after Time ms.
%% @param   Options options
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Start a timer, and send {timeout, TimerRef, Msg} to Dest after
%%          Time ms, where TimerRef is the reference returned from this function.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%%-----------------------------------------------------------------------------
-spec start_timer(
    Time :: non_neg_integer(), Dest :: pid() | atom(), Msg :: term(), _Options :: list()
) -> reference().
start_timer(Time, Dest, Msg, _Options) ->
    timer_manager:start_timer(Time, Dest, Msg).

%%-----------------------------------------------------------------------------
%% @hidden
%% @param   Time time in milliseconds after which to send the timeout message.
%% @param   Dest Pid or server name to which to send the timeout message.
%% @param   Msg Message to send to Dest after Time ms.
%% @param   Options options
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Start a timer, and send {timeout, TimerRef, Msg} to Dest after
%%          Time ms, where TimerRef is the reference returned from this function.
%%
%%          <em><b>Note.</b>  The Options argument is currently ignored.</em>
%%-----------------------------------------------------------------------------
-spec cancel_timer(TimerRef :: reference()) -> ok.
cancel_timer(TimerRef) ->
    timer_manager:cancel_timer(TimerRef).

%%-----------------------------------------------------------------------------
%% @param   Time time in milliseconds after which to send the message.
%% @param   Dest Pid or server name to which to send the message.
%% @param   Msg Message to send to Dest after Time ms.
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Send Msg to Dest after Time ms.
%% @end
%%-----------------------------------------------------------------------------
-spec send_after(Time :: non_neg_integer(), Dest :: pid() | atom(), Msg :: term()) -> reference().
send_after(Time, Dest, Msg) ->
    timer_manager:send_after(Time, Dest, Msg).

%%-----------------------------------------------------------------------------
%% @param   Pid the process pid.
%% @param   Key key used to find process information.
%% @returns process information for the specified pid defined by the specified key.
%% @doc     Return process information.
%%
%% This function returns information about the specified process.
%% The type of information returned is dependent on the specified key.
%%
%% The following keys are supported:
%% <ul>
%%      <li><b>heap_size</b> the number of words used in the heap (integer)</li>
%%      <li><b>stack_size</b> the number of words used in the stack (integer)</li>
%%      <li><b>message_queue_len</b> the number of messages enqueued for the process (integer)</li>
%%      <li><b>memory</b> the estimated total number of bytes in use by the process (integer)</li>
%% </ul>
%% Specifying an unsupported term or atom raises a bad_arg error.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec process_info(Pid :: pid(), Key :: atom()) -> term().
process_info(_Pid, _Key) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Key key used to find system information.
%% @returns system information defined by the specified key.
%% @doc     Return system information.
%%
%% This function returns information about the system on which AtomVM is
%% running. The type of information returned is dependent on the specified key.
%%
%% The following keys are supported on all platforms:
%% <ul>
%%      <li><b>process_count</b> the number of processes running in the node (integer)</li>
%%      <li><b>port_count</b> the number of ports running in the node (integer)</li>
%%      <li><b>atom_count</b> the number of atoms currently allocated (integer)</li>
%%      <li><b>system_architecture</b> the processor and OS architecture (binary)</li>
%%      <li><b>version</b> the version of the AtomVM executable image (binary)</li>
%%      <li><b>wordsize</b> the number of bytes in a machine word on the current platform (integer)</li>
%%      <li><b>schedulers</b> the number of schedulers, equal to the number of online processors (integer)</li>
%%      <li><b>schedulers_online</b> the current number of schedulers (integer)</li>
%% </ul>
%% The following keys are supported on the ESP32 platform:
%% <ul>
%%      <li><b>esp32_free_heap_size</b> the number of (noncontiguous) free bytes in the ESP32 heap (integer)</li>
%%      <li><b>esp_largest_free_block</b> the number of the largest contiguous free bytes in the ESP32 heap (integer)</li>
%%      <li><b>esp_get_minimum_free_size</b> the smallest number of free bytes in the ESP32 heap since boot (integer)</li>
%% </ul>
%%
%% Additional keys may be supported on some platforms that are not documented here.
%%
%% Specifying an unsupported atom key will results in returning the atom 'undefined'.
%%
%% Specifying a term that is not an atom will result in a bad_arg error.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec system_info(Key :: atom()) -> term().
system_info(_Key) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Key key used to change system flag.
%% @param   Value value to change
%% @returns previous value of the flag.
%% @doc     Update system flags.
%%
%% This function allows to modify system flags at runtime.
%%
%% The following key is supported on SMP builds:
%% <ul>
%%       <li><b>schedulers_online</b> the number of schedulers online</li>
%% </ul>
%%
%% Specifying an unsupported atom key will result in a bad_arg error.
%% Specifying a term that is not an atom will result in a bad_arg error.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec system_flag(Key :: atom(), term()) -> term().
system_flag(_Key, _Value) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Data data to compute hash of, as a binary.
%% @returns the md5 hash of the input Data, as a 16-byte binary.
%% @doc     Computes the MD5 hash of an input binary, as defined by
%%          https://www.ietf.org/rfc/rfc1321.txt
%% @end
%%-----------------------------------------------------------------------------
-spec md5(Data :: binary()) -> binary().
md5(Data) when is_binary(Data) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Module Name of module
%% @param   Function Exported function name
%% @param   Args Parameters to pass to function (max 6)
%% @returns Returns the result of Module:Function(Args).
%% @doc     Returns the result of applying Function in Module to Args. The applied
%%          function must be exported from Module. The arity of the function is the
%%          length of Args. Example:
%%          ```> apply(lists, reverse, [[a, b, c]]).
%%          [c,b,a]
%%          > apply(erlang, atom_to_list, ['AtomVM']).
%%          "AtomVM"'''
%%          If the number of arguments are known at compile time, the call is better
%%          written as Module:Function(Arg1, Arg2, ..., ArgN).
%% @end
%%-----------------------------------------------------------------------------
-spec apply(Module :: module(), Function :: function(), Args :: [term()]) -> term().
apply(Module, Function, Args) ->
    case Args of
        [] ->
            Module:Function();
        [Arg1] ->
            Module:Function(Arg1);
        [Arg1, Arg2] ->
            Module:Function(Arg1, Arg2);
        [Arg1, Arg2, Arg3] ->
            Module:Function(Arg1, Arg2, Arg3);
        [Arg1, Arg2, Arg3, Arg4] ->
            Module:Function(Arg1, Arg2, Arg3, Arg4);
        [Arg1, Arg2, Arg3, Arg4, Arg5] ->
            Module:Function(Arg1, Arg2, Arg3, Arg4, Arg5);
        [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6] ->
            Module:Function(Arg1, Arg2, Arg3, Arg4, Arg5, Arg6);
        _ ->
            throw(badarg)
    end.

%%-----------------------------------------------------------------------------
%% @param   Map     the map to test
%% @returns `true' if `Map' is a map; `false', otherwise.
%% @doc     Return `true' if `Map' is a map; `false', otherwise.
%%
%% This function may be used in a guard expression.
%% @end
%%-----------------------------------------------------------------------------
-spec is_map(Map :: map()) -> boolean().
is_map(_Map) ->
    throw(bif_error).

%%-----------------------------------------------------------------------------
%% @param   Map the map
%% @returns the size of the map
%% @throws {badmap, Map}
%% @doc Returns the size of (i.e., the number of entries in) the map
%%
%% This function throws a `{badmap, Map}' exception if `Map' is not a map.
%%
%% This function may be used in a guard expression.
%% @end
%%-----------------------------------------------------------------------------
-spec map_size(Map :: map()) -> non_neg_integer().
map_size(_Map) ->
    throw(bif_error).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to get
%% @param   Map     the map from which to get the value
%% @returns the value in `Map' associated with `Key', if it exists.
%% @throws  {badkey, Key} | {badmap, Map}
%% @doc     Get the value in `Map' associated with `Key', if it exists.
%%
%% This function throws a `{badkey, Key}' exception if 'Key' does not occur in `Map' or
%% a `{badmap, Map}' if `Map' is not a map.
%%
%% This function may be used in a guard expression.
%% @end
%%-----------------------------------------------------------------------------
-spec map_get(Key :: term(), Map :: map()) -> Value :: term().
map_get(_Key, _Map) ->
    throw(bif_error).

%%-----------------------------------------------------------------------------
%% @param   Key     the key
%% @param   Map     the map
%% @returns `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%% @throws  {badmap, Map}
%% @doc     Return `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%%
%% This function throws a `{badmap, Map}' exception if `Map' is not a map.
%%
%% This function may be used in a guard expression.
%% @end
%%-----------------------------------------------------------------------------
-spec map_is_key(Key :: term(), Map :: map()) -> boolean().
map_is_key(_Key, _Map) ->
    throw(bif_error).

%%-----------------------------------------------------------------------------
%% @param   A   any term
%% @param   B   any term
%% @returns `A' if `A < B'; `B', otherwise.
%% @doc     Return the minimum value of two terms
%%
%% Terms are compared using `<' and follow the ordering principles defined in
%% https://www.erlang.org/doc/reference_manual/expressions.html#term-comparisons
%% @end
%%-----------------------------------------------------------------------------
-spec min(A :: any(), B :: any()) -> any().
min(_A, _B) ->
    throw(bif_error).

%%-----------------------------------------------------------------------------
%% @param   A   any term
%% @param   B   any term
%% @returns `A' if `A > B'; `B', otherwise.
%% @doc     Return the maximum value of two terms
%%
%% Terms are compared using `>' and follow the ordering principles defined in
%% https://www.erlang.org/doc/reference_manual/expressions.html#term-comparisons
%% @end
%%-----------------------------------------------------------------------------
-spec max(A :: any(), B :: any()) -> any().
max(_A, _B) ->
    throw(bif_error).

%%-----------------------------------------------------------------------------
%% @param   Type the type of memory to request
%% @returns the amount of memory (in bytes) used of the specified type
%% @doc     Return the amount of memory (in bytes) used of the specified type
%%
%% @end
%%-----------------------------------------------------------------------------
-spec memory(Type :: mem_type()) -> non_neg_integer().
memory(_Type) ->
    throw(nif_error).

%%-----------------------------------------------------------------------------
%% @param   Unit    time unit
%% @returns monotonic time in the specified units
%% @doc     Return the monotonic time in the specified units.
%%
%% Monotonic time varies from system to system, and should not be used
%% to determine, for example the wall clock time.
%%
%% Instead, monotonic time should be used to compute time differences,
%% where the function is guaranteed to return a (not necessarily strictly)
%% monotonically increasing value.
%%
%% For example, on ESP32 system, monotonic time is reported as the difference from
%% the current time and the time the ESP32 device was started, whereas on UNIX
%% systems the value may vary among UNIX systems (e.g., Linux, macOS, FreeBSD).
%% @end
%%-----------------------------------------------------------------------------
-spec monotonic_time(Unit :: time_unit()) -> integer().
monotonic_time(_Unit) ->
    throw(nif_error).
