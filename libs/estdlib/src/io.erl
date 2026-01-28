%
% This file is part of AtomVM.
%
% Copyright 2018-2021 Davide Bettio <davide@uninstall.it>
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
%% @doc An implementation of the Erlang/OTP io interface.
%%
%% This module implements a strict subset of the Erlang/OTP io interface.
%% @end
%%-----------------------------------------------------------------------------
-module(io).

-export([
    requests/1,
    format/1,
    format/2,
    format/3,
    fwrite/1,
    fwrite/2,
    fwrite/3,
    get_line/1,
    put_chars/1,
    put_chars/2,
    scan_erl_exprs/4,
    columns/0,
    columns/1,
    getopts/0,
    getopts/1,
    setopts/1,
    setopts/2,
    printable_range/0
]).

-export_type([device/0, format/0, standard_io/0, standard_error/0, user/0]).

-type format() :: atom() | string() | binary().
-type standard_io() :: standard_io.
-type standard_error() :: standard_error.
-type user() :: user.
-type device() :: atom() | pid() | standard_io() | standard_error() | user().

-type getopt() ::
    {echo, boolean()}
    | {binary, boolean()}
    | {encoding, unicode}
    | {terminal, boolean()}
    | {stdin, boolean()}
    | {stdout, boolean()}
    | {stderr, boolean()}.

%% @private
requests(Requests) ->
    execute_request(group_leader(), {requests, Requests}).

%%-----------------------------------------------------------------------------
%% @equiv columns(standard_io)
%% @doc Returns the number of columns for standard I/O.
%% @end
%%-----------------------------------------------------------------------------
-spec columns() -> {ok, pos_integer()} | {error, enotsup}.
columns() ->
    columns(standard_io).

%%-----------------------------------------------------------------------------
%% @param IODevice IO device to get columns of
%% @doc Returns the number of columns for passed I/O device.
%% @end
%%-----------------------------------------------------------------------------
-spec columns(IODevice :: device()) -> {ok, pos_integer()} | {error, enotsup}.
columns(_IODevice) ->
    {ok, 80}.

%%-----------------------------------------------------------------------------
%% @equiv getopts(standard_io)
%% @doc Get all options for standard I/O
%% @end
%%-----------------------------------------------------------------------------
-spec getopts() -> [getopt()] | {error, Reason :: any()}.
getopts() ->
    getopts(standard_io).

%%-----------------------------------------------------------------------------
%% @param IODevice IO device to get options for
%% @doc Get all options for a given IODevice
%% @end
%%-----------------------------------------------------------------------------
-spec getopts(IODevice :: device()) -> [getopt()] | {error, Reason :: any()}.
getopts(standard_io) ->
    [
        {echo, true},
        {binary, false},
        {encoding, unicode},
        {terminal, true},
        {stdout, true},
        {stderr, true},
        {stdin, true}
    ].

%%-----------------------------------------------------------------------------
%% @equiv setopts(standard_io, Opts)
%% @doc Set options for standard I/O
%% @end
%%-----------------------------------------------------------------------------
-spec setopts(Opts :: [getopt()]) -> ok | {error, Reason :: any()}.
setopts(Opts) ->
    setopts(standard_io, Opts).

%%-----------------------------------------------------------------------------
%% @param IODevice IO device to set options for
%% @param Opts Options to set
%% @doc Set options for a given IODevice. Currently a no-op stub.
%% @end
%%-----------------------------------------------------------------------------
-spec setopts(IODevice :: device(), Opts :: [getopt()]) -> ok | {error, Reason :: any()}.
setopts(_IODevice, _Opts) ->
    %% TODO: Actually implement option setting when needed
    ok.

%%-----------------------------------------------------------------------------
%% @doc Returns the user-requested range of printable Unicode characters.
%% Currently always returns `unicode'
%% @end
%%-----------------------------------------------------------------------------
-spec printable_range() -> unicode | latin1.
printable_range() ->
    unicode.

%% @equiv fwrite(Format)
-spec format(Format :: format()) -> ok.
format(Format) ->
    fwrite(Format).

%% @equiv fwrite(Format, [])
-spec fwrite(Format :: format()) -> ok.
fwrite(Format) ->
    fwrite(Format, []).

%% @equiv fwrite(Format, Data)
-spec format(Format :: format(), Data :: [term()]) -> ok.
format(Format, Data) ->
    fwrite(Format, Data).

%% @equiv fwrite(standard_io, Format, Data)
-spec fwrite(Format :: format(), Data :: [term()]) -> ok.
fwrite(Format, Data) ->
    fwrite(standard_io, Format, Data).

%% @equiv fwrite(IODevice, Format, Data)
-spec format(IODevice :: device(), Format :: format(), Data :: [term()]) -> ok.
format(IODevice, Format, Data) ->
    fwrite(IODevice, Format, Data).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Data format arguments
%% @param   IODevice device to write to
%% @returns ok
%% @doc     Format string and data to IO device.
%%          See io_lib:format/2 for information about
%%          formatting capabilities.
%% @end
%%-----------------------------------------------------------------------------
-spec fwrite(IODevice :: device(), Format :: format(), Args :: list()) -> ok.
fwrite(IODevice, Format, Args) when
    (is_list(Format) orelse is_binary(Format)) andalso is_list(Args)
->
    Msg =
        try
            io_lib:format(Format, Args)
        catch
            _:_ ->
                io_lib:format("Bad format!  Format: ~p Args: ~p~n", [Format, Args])
        end,
    put_chars(IODevice, Msg).

%%-----------------------------------------------------------------------------
%% @param   IODevice device to read from
%% @param   Prompt prompt to print
%% @param   StartLocation start location for reading
%% @param   Options options for tokenizing
%% @returns read tokens or an error
%% @doc     Reads data from IODevice with a given prompt
%% @end
%%-----------------------------------------------------------------------------
-spec scan_erl_exprs(IODevice :: device(), Prompt :: atom() | unicode:chardata(), any(), any()) ->
    {ok, Tokens :: any(), EndLocation :: any()}
    | {eof, EndLocation :: any()}
    | eof
    | {error, any()}.
scan_erl_exprs(IODevice, Prompt, StartLocation, Options) when is_pid(IODevice) ->
    execute_request(
        IODevice, {get_until, unicode, Prompt, erl_scan, tokens, [StartLocation, Options]}
    ).

%%-----------------------------------------------------------------------------
%% @param   Prompt prompt for user input
%% @returns string
%% @doc     Read string from console with prompt.
%% @end
%%-----------------------------------------------------------------------------
-spec get_line(Prompt :: string()) -> string().
get_line(Prompt) ->
    Self = self(),
    case erlang:group_leader() of
        Self ->
            erlang:throw(no_group_leader);
        Leader ->
            Ref = make_ref(),
            Leader ! {io_request, self(), Ref, {get_line, unicode, Prompt}},
            receive
                {io_reply, Ref, Line} -> Line
            end
    end.

%%-----------------------------------------------------------------------------
%% @equiv   put_chars(standard_io, Chars)
%% @param   Chars character(s) to write to console
%% @returns ok
%% @doc     Writes the given character(s) to the console.
%% @end
%%-----------------------------------------------------------------------------
-spec put_chars(Chars :: list() | binary()) -> ok.
put_chars(Chars) ->
    put_chars(standard_io, Chars).

%%-----------------------------------------------------------------------------
%% @param   Device device to send characters to
%% @param   Chars character(s) to write to device
%% @returns ok
%% @doc     Writes the given character(s) to the given device.
%% @end
%%-----------------------------------------------------------------------------
-spec put_chars(Device :: device(), Chars :: list() | binary()) -> ok.
put_chars(standard_io, Chars) ->
    Self = self(),
    case erlang:group_leader() of
        Self ->
            console:print(Chars);
        Leader ->
            execute_request(Leader, {put_chars, unicode, Chars})
    end;
put_chars(standard_error, Chars) ->
    put_chars(standard_io, Chars).

%% @private
convert_request({requests, Requests}) ->
    {requests, [convert_request(Request) || Request <- Requests]};
convert_request(nl) ->
    {put_chars, unicode, "\n"};
convert_request(Other) ->
    Other.

%% @private
execute_request(Device, Request) when is_pid(Device) ->
    Converted = convert_request(Request),
    Ref = make_ref(),
    Device ! {io_request, self(), Ref, Converted},
    receive
        {io_reply, Ref, Result} -> Result
    end.
