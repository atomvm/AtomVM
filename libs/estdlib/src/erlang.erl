%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright 2018 by Fred Dushin <fred@dushin.net>                       %
%                                                                         %
%   This program is free software; you can redistribute it and/or modify  %
%   it under the terms of the GNU Lesser General Public License as        %
%   published by the Free Software Foundation; either version 2 of the    %
%   License, or (at your option) any later version.                       %
%                                                                         %
%   This program is distributed in the hope that it will be useful,       %
%   but WITHOUT ANY WARRANTY; without even the implied warranty of        %
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         %
%   GNU General Public License for more details.                          %
%                                                                         %
%   You should have received a copy of the GNU General Public License     %
%   along with this program; if not, write to the                         %
%   Free Software Foundation, Inc.,                                       %
%   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA .        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    md5/1
]).

%%-----------------------------------------------------------------------------
%% @param   Time time in milliseconds after which to send the timeout message.
%% @param   Dest Pid or server name to which to send the timeout message.
%% @param   Msg Message to send to Dest after Time ms.
%% @returns a reference that can be used to cancel the timer, if desired.
%% @doc     Start a timer, and send {timeout, TimerRef, Msg} to Dest after
%%          Time ms, where TimerRef is the reference returned from this function.
%% @end
%%-----------------------------------------------------------------------------
-spec start_timer(non_neg_integer(), pid() | atom(), term()) -> reference().
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
-spec start_timer(non_neg_integer(), pid() | atom(), term(), list()) -> reference().
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
-spec send_after(non_neg_integer(), pid() | atom(), term()) -> reference().
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
%% </ul>
%% The following keys are supported on the ESP32 platform:
%% <ul>
%%      <li><b>esp32_free_heap_size</b> the number of (noncontiguous) free bytes in the ESP32 heap (integer)</li>
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
%% @param   Data data to compute hash of, as a binary.
%% @returns the md5 hash of the input Data, as a 16-byte binary.
%% @doc     Computes the MD5 hash of an input binary, as defined by
%%          https://www.ietf.org/rfc/rfc1321.txt
%% @end
%%-----------------------------------------------------------------------------
-spec md5(Data :: binary()) -> binary().
md5(Data) when is_binary(Data) ->
    throw(nif_error).

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
