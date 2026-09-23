%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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
%% @doc GPIO driver module for generic UNIX (Linux), backed sysfs
%%
%% The sysfs base directory defaults to `/sys/class/gpio' and can be overridden
%% with {@link set_sysfs_base/1} (for testing).
%% @end
%%-----------------------------------------------------------------------------
-module(gpio).

-export([
    init/1,
    deinit/1,
    set_pin_mode/2,
    digital_write/2,
    digital_read/1,
    set_sysfs_base/1
]).

-type pin() :: non_neg_integer().
%% A Linux GPIO number.
-type direction() :: input | output | output_od.
%% Pin direction. `output_od' (open drain) is treated as `output' by sysfs.
-type low_level() :: low | 0.
-type high_level() :: high | 1.
-type level() :: low_level() | high_level().
%% A pin level, as an atom or integer.

-define(DEFAULT_SYSFS_BASE, "/sys/class/gpio").

%%-----------------------------------------------------------------------------
%% @param   Pin GPIO number to export
%% @returns `ok' or `{error, Reason}'
%% @doc     Export a pin (make it available under the sysfs base). Exporting an
%%          already-exported pin is not an error.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Pin :: pin()) -> ok | {error, atom()}.
init(Pin) ->
    %% Writing an already-exported pin fails with EBUSY; treat that as success.
    case write_file(export_path(), integer_to_list(Pin)) of
        {error, ebusy} -> ok;
        Other -> Other
    end.

%%-----------------------------------------------------------------------------
%% @param   Pin GPIO number to unexport
%% @returns `ok' or `{error, Reason}'
%% @doc     Unexport a previously exported pin.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Pin :: pin()) -> ok | {error, atom()}.
deinit(Pin) ->
    write_file(unexport_path(), integer_to_list(Pin)).

%%-----------------------------------------------------------------------------
%% @param   Pin GPIO number
%% @param   Mode `input', `output' or `output_od'
%% @returns `ok' or `{error, Reason}'
%% @doc     Set the direction of a pin, exporting it first if necessary.
%% @end
%%-----------------------------------------------------------------------------
-spec set_pin_mode(Pin :: pin(), Mode :: direction()) -> ok | {error, atom()}.
set_pin_mode(Pin, Mode) ->
    case init(Pin) of
        ok ->
            write_file(attr_path(Pin, "direction"), direction_string(Mode));
        {error, _Reason} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Pin GPIO number
%% @param   Level `high'/`1' or `low'/`0'
%% @returns `ok' or `{error, Reason}'
%% @doc     Set the output level of a pin.
%% @end
%%-----------------------------------------------------------------------------
-spec digital_write(Pin :: pin(), Level :: level()) -> ok | {error, atom()}.
digital_write(Pin, Level) ->
    write_file(attr_path(Pin, "value"), level_string(Level)).

%%-----------------------------------------------------------------------------
%% @param   Pin GPIO number
%% @returns `high', `low' or `{error, Reason}'
%% @doc     Read the level of a pin.
%% @end
%%-----------------------------------------------------------------------------
-spec digital_read(Pin :: pin()) -> high | low | {error, atom()}.
digital_read(Pin) ->
    case read_file(attr_path(Pin, "value")) of
        {ok, <<$1, _/binary>>} -> high;
        {ok, <<$0, _/binary>>} -> low;
        {ok, _Other} -> {error, unexpected_value};
        {error, _Reason} = Error -> Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Dir base sysfs directory (default `/sys/class/gpio')
%% @returns `ok'
%% @doc     Override the sysfs GPIO base directory (mainly for testing).
%%
%%          The override is stored in the calling process' dictionary, so it
%%          only affects GPIO calls made from that same process.
%% @end
%%-----------------------------------------------------------------------------
-spec set_sysfs_base(Dir :: string()) -> ok.
set_sysfs_base(Dir) ->
    _ = erlang:put({?MODULE, sysfs_base}, Dir),
    ok.

%%-----------------------------------------------------------------------------
%% internal
%%-----------------------------------------------------------------------------
sysfs_base() ->
    case erlang:get({?MODULE, sysfs_base}) of
        undefined -> ?DEFAULT_SYSFS_BASE;
        Dir -> Dir
    end.

export_path() ->
    sysfs_base() ++ "/export".

unexport_path() ->
    sysfs_base() ++ "/unexport".

attr_path(Pin, Attr) ->
    sysfs_base() ++ "/gpio" ++ integer_to_list(Pin) ++ "/" ++ Attr.

direction_string(input) -> "in";
direction_string(output) -> "out";
direction_string(output_od) -> "out".

level_string(high) -> "1";
level_string(1) -> "1";
level_string(low) -> "0";
level_string(0) -> "0".

write_file(Path, Data) ->
    case atomvm:posix_open(Path, [o_wronly]) of
        {ok, Fd} ->
            Result =
                case atomvm:posix_write(Fd, iolist_to_binary(Data)) of
                    {ok, _Count} -> ok;
                    {error, _Reason} = Error -> Error
                end,
            _ = atomvm:posix_close(Fd),
            Result;
        {error, _Reason} = Error ->
            Error
    end.

read_file(Path) ->
    case atomvm:posix_open(Path, [o_rdonly]) of
        {ok, Fd} ->
            Result =
                case atomvm:posix_read(Fd, 8) of
                    {ok, Data} -> {ok, Data};
                    eof -> {ok, <<>>};
                    {error, _Reason} = Error -> Error
                end,
            _ = atomvm:posix_close(Fd),
            Result;
        {error, _Reason} = Error ->
            Error
    end.
