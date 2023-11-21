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

%% @hidden
-module(logger_std_h).

-export([
    log/2
]).

%% @private
format_msg({report, Report}) when is_map(Report) ->
    io_lib:format("~p", [Report]);
format_msg({Format, Args}) when is_list(Format) andalso is_list(Args) ->
    io_lib:format(Format, Args).

%% @hidden
log(LogEvent, _Config) ->
    #{
        level := Level,
        msg := Msg,
        pid := Pid,
        timestamp := Timestamp,
        meta := MetaData
    } = LogEvent,
    io:format("~s [~p] ~p ~s~s~s~n", [
        make_timestamp(Timestamp),
        Level,
        Pid,
        maybe_format_mfa(MetaData),
        maybe_format_file_line(MetaData),
        format_msg(Msg)
    ]).

%% @private
make_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Timestamp, microsecond),
    io_lib:format("~p-~s-~sT~s:~s:~s.~sZ", [
        Year,
        maybe_pad(Month),
        maybe_pad(Day),
        maybe_pad(Hour),
        maybe_pad(Minute),
        maybe_pad(Second),
        maybe_pad_100((Timestamp rem 1000000) div 1000)
    ]).

%% @private
maybe_pad(N) when N >= 0 andalso N < 10 ->
    [$0 | integer_to_list(N)];
maybe_pad(N) ->
    integer_to_list(N).

%% @private
maybe_pad_100(N) when N >= 0 andalso N < 10 ->
    [$0, $0 | integer_to_list(N)];
maybe_pad_100(N) when N >= 0 andalso N < 100 ->
    [$0 | integer_to_list(N)];
maybe_pad_100(N) ->
    integer_to_list(N).

%% @private
maybe_format_mfa(#{location := #{mfa := {Module, FunctionName, FunctionArity}}} = _MetaData) ->
    io_lib:format("~p:~p/~p ", [Module, FunctionName, FunctionArity]);
maybe_format_mfa(_MetaData) ->
    "".

%% @private
maybe_format_file_line(#{location := #{file := File, line := Line}} = _MetaData) ->
    io_lib:format("(~s:~p) ", [File, Line]);
maybe_format_file_line(_MetaData) ->
    "".
