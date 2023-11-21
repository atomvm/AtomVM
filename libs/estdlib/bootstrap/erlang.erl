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
    memory/1,
    get/1,
    put/2,
    erase/1,
    function_exported/3,
    display/1,
    list_to_atom/1,
    list_to_existing_atom/1,
    list_to_binary/1,
    list_to_integer/1,
    list_to_tuple/1,
    iolist_to_binary/1,
    binary_to_atom/2,
    binary_to_integer/1,
    binary_to_list/1,
    atom_to_binary/2,
    atom_to_list/1,
    float_to_binary/1,
    float_to_binary/2,
    float_to_list/1,
    float_to_list/2,
    integer_to_binary/1,
    integer_to_binary/2,
    integer_to_list/1,
    integer_to_list/2,
    fun_to_list/1,
    pid_to_list/1,
    ref_to_list/1,
    register/2,
    unregister/1,
    whereis/1,
    spawn/1,
    spawn/3,
    spawn_link/1,
    spawn_link/3,
    spawn_opt/2,
    spawn_opt/4,
    link/1,
    unlink/1,
    make_ref/0,
    send/2,
    monitor/2,
    demonitor/1,
    demonitor/2,
    exit/1,
    exit/2,
    open_port/2,
    system_time/1,
    group_leader/0,
    group_leader/2,
    process_flag/2,
    get_module_info/1,
    get_module_info/2,
    processes/0,
    is_process_alive/1,
    garbage_collect/0,
    garbage_collect/1,
    binary_to_term/1,
    term_to_binary/1,
    timestamp/0,
    universaltime/0,
    localtime/0
]).

-export_type([
    time_unit/0,
    timestamp/0
]).

%%
%% TODO Correct the following bugs
%% * cancel_timer should be renamed cancel, per the OTP documentation
%% * return value needs to be {ok, cancel} or {error, Reason}
%% * review API documentation for timer functions in this module
%%

-type mem_type() :: binary.
-type time_unit() :: second | millisecond | microsecond.
-type timestamp() :: {
    MegaSecs :: non_neg_integer(), Secs :: non_neg_integer(), MicroSecs :: non_neg_integer
}.

-type float_format_option() ::
    {decimals, Decimals :: 0..57}
    | {scientific, Decimals :: 0..57}
    | compact.

-type demonitor_option() :: flush | {flush, boolean()} | info | {info, boolean()}.

-type heap_growth_strategy() ::
    bounded_free
    | minimum
    | fibonacci.

-type spawn_option() ::
    {min_heap_size, pos_integer()}
    | {max_heap_size, pos_integer()}
    | {atomvm_heap_growth, heap_growth_strategy()}
    | link
    | monitor.

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
%%      <li><b>heap_size</b> the number of words used in the heap (integer), including the stack but excluding fragments</li>
%%      <li><b>total_heap_size</b> the number of words used in the heap (integer) including fragments</li>
%%      <li><b>stack_size</b> the number of words used in the stack (integer)</li>
%%      <li><b>message_queue_len</b> the number of messages enqueued for the process (integer)</li>
%%      <li><b>memory</b> the estimated total number of bytes in use by the process (integer)</li>
%%      <li><b>links</b> the list of linked processes</li>
%% </ul>
%% Specifying an unsupported term or atom raises a bad_arg error.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec process_info
    (Pid :: pid(), heap_size) -> {heap_size, non_neg_integer()};
    (Pid :: pid(), total_heap_size) -> {total_heap_size, non_neg_integer()};
    (Pid :: pid(), stack_size) -> {stack_size, non_neg_integer()};
    (Pid :: pid(), message_queue_len) -> {message_queue_len, non_neg_integer()};
    (Pid :: pid(), memory) -> {memory, non_neg_integer()};
    (Pid :: pid(), links) -> {links, [pid()]}.
process_info(_Pid, _Key) ->
    erlang:nif_error(undefined).

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
    erlang:nif_error(undefined).

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
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Data data to compute hash of, as a binary.
%% @returns the md5 hash of the input Data, as a 16-byte binary.
%% @doc     Computes the MD5 hash of an input binary, as defined by
%%          https://www.ietf.org/rfc/rfc1321.txt
%% @end
%%-----------------------------------------------------------------------------
-spec md5(Data :: binary()) -> binary().
md5(Data) when is_binary(Data) ->
    crypto:hash(md5, Data).

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
            error(badarg)
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
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Map the map
%% @returns the size of the map
%% @doc Returns the size of (i.e., the number of entries in) the map
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%%
%% This function may be used in a guard expression.
%% @end
%%-----------------------------------------------------------------------------
-spec map_size(Map :: map()) -> non_neg_integer().
map_size(_Map) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Key     the key to get
%% @param   Map     the map from which to get the value
%% @returns the value in `Map' associated with `Key', if it exists.
%% @doc     Get the value in `Map' associated with `Key', if it exists.
%%
%% This function raises a `{badkey, Key}' error if 'Key' does not occur in
%% `Map' or a `{badmap, Map}' if `Map' is not a map.
%%
%% This function may be used in a guard expression.
%% @end
%%-----------------------------------------------------------------------------
-spec map_get(Key :: term(), Map :: map()) -> Value :: term().
map_get(_Key, _Map) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Key     the key
%% @param   Map     the map
%% @returns `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%% @doc     Return `true' if `Key' is associated with a value in `Map'; `false', otherwise.
%%
%% This function raises a `{badmap, Map}' error if `Map' is not a map.
%%
%% This function may be used in a guard expression.
%% @end
%%-----------------------------------------------------------------------------
-spec map_is_key(Key :: term(), Map :: map()) -> boolean().
map_is_key(_Key, _Map) ->
    erlang:nif_error(undefined).

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
    erlang:nif_error(undefined).

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
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Type the type of memory to request
%% @returns the amount of memory (in bytes) used of the specified type
%% @doc     Return the amount of memory (in bytes) used of the specified type
%%
%% @end
%%-----------------------------------------------------------------------------
-spec memory(Type :: mem_type()) -> non_neg_integer().
memory(_Type) ->
    erlang:nif_error(undefined).

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
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Key     key in the process dictionary
%% @returns value associated with this key or undefined
%% @doc     Return a value associated with a given key in the process dictionary
%% @end
%%-----------------------------------------------------------------------------
-spec get(Key :: any()) -> any().
get(_Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Key     key to add to the process dictionary
%% @param   Value   value to store in the process dictionary
%% @returns the previous value associated with this key or undefined
%% @doc     Store a value with a given key in the process dictionary.
%% @end
%%-----------------------------------------------------------------------------
-spec put(Key :: any(), Value :: any()) -> any().
put(_Key, _Value) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Key     key to erase from the process dictionary
%% @returns the previous value associated with this key or undefined
%% @doc     Erase a key from the process dictionary.
%% @end
%%-----------------------------------------------------------------------------
-spec erase(Key :: any()) -> any().
erase(_Key) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      module to test
%% @param   Function    function to test
%% @param   Arity       arity to test
%% @returns `true' if Module exports a Function with this Arity
%% @doc     Determine if a function is exported
%% @end
%%-----------------------------------------------------------------------------
-spec function_exported(Module :: module(), Function :: atom(), Arity :: arity()) -> boolean().
function_exported(_Module, _Function, _Arity) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Term    term to print
%% @returns `true'
%% @doc     Print a term to stdout.
%% @end
%%-----------------------------------------------------------------------------
-spec display(Term :: any()) -> true.
display(_Term) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   String  string to convert to an atom
%% @returns an atom from the string
%% @see     list_to_existing_atom/1
%% @doc     Convert a string into an atom.
%% Unlike Erlang/OTP 20+, atoms are limited to ISO-8859-1 characters. The VM
%% currently aborts if passed unicode characters.
%% Atoms are also limited to 255 characters. Errors with system_limit_atom if
%% the passed string is longer.
%% @end
%%-----------------------------------------------------------------------------
-spec list_to_atom(String :: string()) -> atom().
list_to_atom(_String) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   String  string to convert to an atom
%% @returns an atom from the string
%% @see     list_to_atom/1
%% @doc     Convert a string into an atom.
%% This function will error with badarg if the atom does not exist
%% @end
%%-----------------------------------------------------------------------------
-spec list_to_existing_atom(String :: string()) -> atom().
list_to_existing_atom(_String) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   IOList   iolist to convert to binary
%% @returns a binary composed of bytes and binaries from the list
%% @doc     Convert a list into a binary.
%% Errors with `badarg' if the list is not an iolist.
%% @end
%%-----------------------------------------------------------------------------
-spec list_to_binary(IOList :: iolist()) -> binary().
list_to_binary(_IOList) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   String  string to convert to integer
%% @returns an integer value from its string representation
%% @doc     Convert a string (list of characters) to integer.
%% Errors with `badarg' if the string is not a representation of an integer.
%% @end
%%-----------------------------------------------------------------------------
-spec list_to_integer(String :: string()) -> integer().
list_to_integer(_String) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   List    list to convert to tuple
%% @returns a tuple with elements of the list
%% @doc     Convert a list to a tuple with the same size.
%% @end
%%-----------------------------------------------------------------------------
-spec list_to_tuple(List :: [any()]) -> tuple().
list_to_tuple(_List) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   IOList  IO list to convert to binary
%% @returns a binary with the bytes of the IO list
%% @doc     Convert an IO list to binary.
%% @end
%%-----------------------------------------------------------------------------
-spec iolist_to_binary(IOList :: iolist()) -> binary().
iolist_to_binary(_IOList) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Binary  Binary to convert to atom
%% @param   Encoding encoding for conversion
%% @returns an atom from passed binary
%% @doc     Convert a binary to atom.
%% Only latin1 encoded is supported.
%% @end
%%-----------------------------------------------------------------------------
-spec binary_to_atom(Binary :: binary(), Encoding :: latin1) -> atom().
binary_to_atom(_Binary, _Encoding) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Binary  Binary to parse for integer
%% @returns the integer represented by the binary
%% @doc     Parse the text in a given binary as an integer.
%% @end
%%-----------------------------------------------------------------------------
-spec binary_to_integer(Binary :: binary()) -> integer().
binary_to_integer(_Binary) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Binary  Binary to convert to list
%% @returns a list of bytes from the binary
%% @doc     Convert a binary to a list of bytes.
%% @end
%%-----------------------------------------------------------------------------
-spec binary_to_list(Binary :: binary()) -> [byte()].
binary_to_list(_Binary) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Atom        Atom to convert
%% @param   Encoding    Encoding for conversion
%% @returns a binary with the atom's name
%% @doc     Convert an atom to a binary.
%% Only latin1 encoding is supported.
%% @end
%%-----------------------------------------------------------------------------
-spec atom_to_binary(Atom :: atom(), Encoding :: latin1) -> binary().
atom_to_binary(_Atom, _Encoding) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Atom    Atom to convert
%% @returns a string with the atom's name
%% @doc     Convert an atom to a string.
%% @end
%%-----------------------------------------------------------------------------
-spec atom_to_list(Atom :: atom()) -> string().
atom_to_list(_Atom) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Float   Float to convert
%% @returns a binary with a text representation of the float
%% @doc     Convert a float to a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec float_to_binary(Float :: float()) -> binary().
float_to_binary(_Float) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Float   Float to convert
%% @param   Options Options for conversion
%% @returns a binary with a text representation of the float
%% @doc     Convert a float to a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec float_to_binary(Float :: float(), Options :: [float_format_option()]) -> binary().
float_to_binary(_Float, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Float   Float to convert
%% @returns a string with a text representation of the float
%% @doc     Convert a float to a string.
%% @end
%%-----------------------------------------------------------------------------
-spec float_to_list(Float :: float()) -> string().
float_to_list(_Float) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Float   Float to convert
%% @param   Options Options for conversion
%% @returns a string with a text representation of the float
%% @doc     Convert a float to a string.
%% @end
%%-----------------------------------------------------------------------------
-spec float_to_list(Float :: float(), Options :: [float_format_option()]) -> string().
float_to_list(_Float, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Integer integer to convert to a binary
%% @returns a binary with a text representation of the integer
%% @doc     Convert an integer to a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec integer_to_binary(Integer :: integer()) -> binary().
integer_to_binary(_Integer) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Integer integer to convert to a binary
%% @param   Base    base for representation
%% @returns a binary with a text representation of the integer
%% @doc     Convert an integer to a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec integer_to_binary(Integer :: integer(), Base :: 2..36) -> binary().
integer_to_binary(_Integer, _Base) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Integer integer to convert to a string
%% @returns a string representation of the integer
%% @doc     Convert an integer to a string.
%% @end
%%-----------------------------------------------------------------------------
-spec integer_to_list(Integer :: integer()) -> string().
integer_to_list(_Integer) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Integer integer to convert to a string
%% @param   Base    base for representation
%% @returns a string representation of the integer
%% @doc     Convert an integer to a string.
%% @end
%%-----------------------------------------------------------------------------
-spec integer_to_list(Integer :: integer(), Base :: 2..36) -> string().
integer_to_list(_Integer, _Base) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Fun     function to convert to a string
%% @returns a string representation of the function
%% @doc     Create a string representing a function.
%% @end
%%-----------------------------------------------------------------------------
-spec fun_to_list(Fun :: function()) -> string().
fun_to_list(_Fun) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Pid     pid to convert to a string
%% @returns a string representation of the pid
%% @doc     Create a string representing a pid.
%% @end
%%-----------------------------------------------------------------------------
-spec pid_to_list(Pid :: pid()) -> string().
pid_to_list(_Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Ref     reference to convert to a string
%% @returns a string representation of the reference
%% @doc     Create a string representing a reference.
%% @end
%%-----------------------------------------------------------------------------
-spec ref_to_list(Ref :: reference()) -> string().
ref_to_list(_Ref) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Name    name of the process to register
%% @param   Pid     pid of the process to register
%% @returns `true'
%% @doc     Register a name for a given process.
%% Processes can be registered with several names.
%% Unlike Erlang/OTP, ports are not distinguished from processes.
%% Errors with `badarg' if the name is already registered.
%% @end
%%-----------------------------------------------------------------------------
-spec register(Name :: atom(), Pid :: pid()) -> true.
register(_Name, _Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Name    name to unregister
%% @returns `true'
%% @doc     Lookup a process by name.
%% Unlike Erlang/OTP, ports are not distinguished from processes.
%% Errors with `badarg' if the name is not registered.
%% @end
%%-----------------------------------------------------------------------------
-spec unregister(Name :: atom()) -> true.
unregister(_Name) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Name    name of the process to locate
%% @returns `undefined' or the pid of the registered process
%% @doc     Lookup a process by name.
%% @end
%%-----------------------------------------------------------------------------
-spec whereis(Name :: atom()) -> pid() | undefined.
whereis(_Name) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Function    function to create a process from
%% @returns pid of the new process
%% @doc     Create a new process
%% @end
%%-----------------------------------------------------------------------------
-spec spawn(Function :: function()) -> pid().
spawn(Function) ->
    erlang:spawn_opt(Function, []).

%%-----------------------------------------------------------------------------
%% @param   Module      module of the function to create a process from
%% @param   Function    name of the function to create a process from
%% @param   Args        arguments to pass to the function to create a process from
%% @returns pid of the new process
%% @doc     Create a new process by calling exported Function from Module with Args.
%% @end
%%-----------------------------------------------------------------------------
-spec spawn(Module :: module(), Function :: atom(), Args :: [any()]) -> pid().
spawn(Module, Function, Args) ->
    erlang:spawn_opt(Module, Function, Args, []).

%%-----------------------------------------------------------------------------
%% @param   Function    function to create a process from
%% @returns pid of the new process
%% @doc     Create a new process and link it.
%% @end
%%-----------------------------------------------------------------------------
-spec spawn_link(Function :: function()) -> pid().
spawn_link(Function) ->
    erlang:spawn_opt(Function, [link]).

%%-----------------------------------------------------------------------------
%% @param   Module      module of the function to create a process from
%% @param   Function    name of the function to create a process from
%% @param   Args        arguments to pass to the function to create a process from
%% @returns pid of the new process
%% @doc     Create a new process by calling exported Function from Module with Args
%% and link it.
%% @end
%%-----------------------------------------------------------------------------
-spec spawn_link(Module :: module(), Function :: atom(), Args :: [any()]) -> pid().
spawn_link(Module, Function, Args) ->
    erlang:spawn_opt(Module, Function, Args, [link]).

%%-----------------------------------------------------------------------------
%% @param   Function    function to create a process from
%% @param   Options     additional options.
%% @returns pid of the new process
%% @doc     Create a new process.
%% @end
%%-----------------------------------------------------------------------------
-spec spawn_opt(Function :: function(), Options :: [spawn_option()]) ->
    pid() | {pid(), reference()}.
spawn_opt(_Name, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module      module of the function to create a process from
%% @param   Function    name of the function to create a process from
%% @param   Args        arguments to pass to the function to create a process from
%% @param   Options     additional options.
%% @returns pid of the new process
%% @doc     Create a new process by calling exported Function from Module with Args.
%% @end
%%-----------------------------------------------------------------------------
-spec spawn_opt(
    Module :: module(), Function :: atom(), Args :: [any()], Options :: [spawn_option()]
) -> pid() | {pid(), reference()}.
spawn_opt(_Module, _Function, _Args, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Pid         process to link to
%% @returns `true'
%% @doc     Link current process with a given process.
%% @end
%%-----------------------------------------------------------------------------
-spec link(Pid :: pid()) -> true.
link(_Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Pid         process to unlink from
%% @returns `true'
%% @doc     Unlink current process from a given process.
%% @end
%%-----------------------------------------------------------------------------
-spec unlink(Pid :: pid()) -> true.
unlink(_Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns a new reference
%% @doc     Create a new reference
%% @end
%%-----------------------------------------------------------------------------
-spec make_ref() -> reference().
make_ref() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Pid     process to send the message to
%% @param   Message message to send
%% @returns the sent message
%% @doc     Send a message to a given process
%% @end
%%-----------------------------------------------------------------------------
-spec send(Pid :: pid(), Message :: Message) -> Message.
send(_Pid, _Message) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Type    type of monitor to create
%% @param   Pid     pid of the object to monitor
%% @returns a monitor reference
%% @doc     Create a monitor on a process or on a port.
%% When the process or the port terminates, the following message is sent to
%% the caller of this function:
%% ```
%% {'DOWN', MonitorRef, Type, Pid, Reason}
%% '''
%% Unlike Erlang/OTP, monitors are only supported for processes and ports.
%% @end
%%-----------------------------------------------------------------------------
-spec monitor(Type :: process | port, Pid :: pid()) -> reference().
monitor(_Type, _Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Monitor reference of monitor to remove
%% @returns `true'
%% @doc     Remove a monitor
%% @end
%%-----------------------------------------------------------------------------
-spec demonitor(Monitor :: reference()) -> true.
demonitor(_Monitor) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Monitor reference of monitor to remove
%% @returns `true'
%% @doc     Remove a monitor, with options.
%% If `flush', monitor messages are flushed and guaranteed to not be received.
%% If `info', return `true' if monitor was removed, `false' if it was not found.
%% If both options are provivded, return `false' if flush was needed.
%% @end
%%-----------------------------------------------------------------------------
-spec demonitor(Monitor :: reference(), Options :: [demonitor_option()]) -> boolean().
demonitor(_Monitor, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Reason  reason for exit
%% @doc     Raises an exception of class `exit' with reason `Reason'.
%% The exception can be caught. If it is not, the process exits.
%% If the exception is not caught the signal is sent to linked processes.
%% In this case, if `Reason' is `kill', it is not transformed into `killed' and
%% linked processes can trap it (unlike `exit/2').
%% @end
%%-----------------------------------------------------------------------------
-spec exit(Reason :: any()) -> no_return().
exit(_Reason) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Process target process
%% @param   Reason  reason for exit
%% @returns `true'
%% @doc     Send an exit signal to target process.
%% The consequences of the exit signal depends on `Reason', on whether
%% `Process' is self() or another process and whether target process is
%% trapping exit.
%% If `Reason' is not `kill' nor `normal':
%% <ul>
%%     <li>If target process is not trapping exits, it exits with `Reason'</li>
%%     <li>If traget process is trapping exits, it receives a message
%%         ``{'EXIT', From, Reason}'' where `From' is the caller of `exit/2'.</li>
%% </ul>
%% If `Reason' is `kill', the target process exits with `Reason' changed to
%% `killed'.
%% If `Reason' is `normal' and `Process' is not `self()':
%% <ul>
%%     <li>If target process is not trapping exits, nothing happens.</li>
%%     <li>If traget process is trapping exits, it receives a message
%%         ``{'EXIT', From, normal}'' where `From' is the caller of `exit/2'.</li>
%% </ul>
%% If `Reason' is `normal' and `Process' is `self()':
%% <ul>
%%     <li>If target process is not trapping exits, it exits with `normal'.</li>
%%     <li>If traget process is trapping exits, it receives a message
%%         ``{'EXIT', From, normal}'' where `From' is the caller of `exit/2'.</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------
-spec exit(Process :: pid(), Reason :: any()) -> true.
exit(_Process, _Reason) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   PortName    Tuple {spawn, Name} identifying the port
%% @param   Options     Options, meaningful for the port
%% @returns A pid identifying the open port
%% @doc     Open a port.
%% Unlike Erlang/OTP, ports are identified by pids.
%% @end
%%-----------------------------------------------------------------------------
-spec open_port(PortName :: {spawn, iodata()}, Options :: [any()] | map()) -> pid().
open_port(_PortName, _Options) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Unit    Unit to return system time in
%% @returns An integer representing system time
%% @doc     Get the current system time in provided unit.
%% @end
%%-----------------------------------------------------------------------------
-spec system_time(Unit :: time_unit()) -> non_neg_integer().
system_time(_Unit) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns Pid of group leader or self() if no group leader is set.
%% @doc     Return the pid of the group leader of caller.
%% @end
%%-----------------------------------------------------------------------------
-spec group_leader() -> pid().
group_leader() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Leader  pid of process to set as leader
%% @param   Pid     pid of process to set a Leader
%% @returns `true'
%% @doc Set the group leader for a given process.
%% @end
%%-----------------------------------------------------------------------------
-spec group_leader(Leader :: pid(), Pid :: pid()) -> true.
group_leader(_Leader, _Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Flag    flag to change
%% @param   Value   new value of the flag
%% @returns Previous value of the flag
%% @doc     Set a flag for the current process.
%% When `trap_exit' is true, exit signals are converted to messages
%% ```
%% {'EXIT', From, Reason}
%% '''
%% and the process does not exit if `Reason' is not `normal'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec process_flag(Flag :: trap_exit, Value :: boolean()) -> pid().
process_flag(_Flag, _Value) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module  module to get info for
%% @returns A list of module info tuples
%% @doc     Get info for a given module.
%% This function is not meant to be called directly but through
%% `Module:module_info/0' exported function.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_module_info(Module :: atom()) -> [{atom(), any()}].
get_module_info(_Module) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Module  module to get info for
%% @param   InfoKey info to get
%% @returns A term representing info for given module
%% @doc     Get specific info for a given module.
%% This function is not meant to be called directly but through
%% `Module:module_info/1' exported function.
%% Supported info keys are `module', `exports', `compile' and `attributes'.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_module_info(Module :: atom(), InfoKey :: atom()) -> any().
get_module_info(_Module, _InfoKey) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A list of pids of all processes
%% @doc     Return a list of all current processes.
%% Compared to Erlang/OTP, this function also returns native processes (ports).
%% @end
%%-----------------------------------------------------------------------------
-spec processes() -> [pid()].
processes() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns `true' if the process is alive, `false' otherwise
%% @param   Pid     pid of the process to test
%% @doc     Determine if a process is alive
%% @end
%%-----------------------------------------------------------------------------
-spec is_process_alive(Pid :: pid()) -> boolean().
is_process_alive(_Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns `true'
%% @doc     Run a garbage collect in current process
%% @end
%%-----------------------------------------------------------------------------
-spec garbage_collect() -> true.
garbage_collect() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns `true' or `false' if the process no longer exists
%% @param   Pid     pid of the process to garbage collect
%% @doc     Run a garbage collect in a given process.
%% The function returns before the garbage collect actually happens.
%% @end
%%-----------------------------------------------------------------------------
-spec garbage_collect(Pid :: pid()) -> boolean().
garbage_collect(_Pid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A term decoded from passed binary
%% @param   Binary  binary to decode
%% @doc Decode a term that was previously encodes with `term_to_binary/1'
%% This function should be mostly compatible with its Erlang/OTP counterpart.
%% Unlike modern Erlang/OTP, resources are currently serialized as empty
%% binaries and cannot be unserialized.
%% @end
%%-----------------------------------------------------------------------------
-spec binary_to_term(Binary :: binary()) -> any().
binary_to_term(_Binary) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A binary encoding passed term.
%% @param   Term    term to encode
%% @doc Encode a term to a binary that can later be decoded with `binary_to_term/1'.
%% This function should be mostly compatible with its Erlang/OTP counterpart.
%% Unlike modern Erlang/OTP, resources are currently serialized as empty
%% binaries.
%% @end
%%-----------------------------------------------------------------------------
-spec term_to_binary(Term :: any()) -> binary().
term_to_binary(_Term) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A tuple representing the current timestamp.
%% @see monotonic_time/1
%% @see system_time/1
%% @doc Return the timestamp in `{MegaSec, Sec, MicroSec}' format.
%% This the old format returned by `erlang:now/0'. Please note that the latter
%% which is deprecated in Erlang/OTP is not implemented by AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec timestamp() -> erlang:timestamp().
timestamp() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A tuple representing the current universal time.
%% @see localtime/0
%% @doc Return the current time and day for UTC.
%% @end
%%-----------------------------------------------------------------------------
-spec universaltime() -> calendar:datetime().
universaltime() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns A tuple representing the current local time.
%% @see universaltime/0
%% @doc Return the current time and day for system local timezone.
%% @end
%%-----------------------------------------------------------------------------
-spec localtime() -> calendar:datetime().
localtime() ->
    erlang:nif_error(undefined).
