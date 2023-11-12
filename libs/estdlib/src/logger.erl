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

%%-----------------------------------------------------------------------------
%% @doc A naive implementation of the Erlang/OTP logger interface.
%%
%% This module implements a strict subset of the Erlang/OTP logger
%% interface, supporting operations for logging messages to various
%% log handlers.  A default handler (`logger_std_h') supports logging to the console.
%%
%% This module is designed to be API-compatible with the Erlang/OTP logger API,
%% with exceptions noted below.  Users can use macros defined in the Erlang/OTP
%% `logger.hrl' header for logging messages.
%%
%% Limitations include but are not limited to:
%% <ul>
%%     <li>No support for logging filters</li>
%%     <li>No support for logging formatters</li>
%%     <li>No API support for logger configuration; all configuration
%%          must be done at initialization of the `logger_manager'</li>
%%     <li>No support for throttling or compacting sequences of repeated
%%          log messages</li>
%% </ul>
%% @end
%%-----------------------------------------------------------------------------

-module(logger).

-compile({no_auto_import, [error/2, error/3]}).

-export([
    allow/2,
    emergency/1, emergency/2, emergency/3,
    alert/1, alert/2, alert/3,
    critical/1, critical/2, critical/3,
    error/1, error/2, error/3,
    warning/1, warning/2, warning/3,
    notice/1, notice/2, notice/3,
    info/1, info/2, info/3,
    debug/1, debug/2, debug/3,
    log/2, log/3, log/4,
    compare/2,
    macro_log/3, macro_log/4, macro_log/5
]).

-type level() :: emergency | alert | critical | error | warning | notice | info | debug.
-export_type([level/0]).

-type string_or_report() :: string() | map().

%%-----------------------------------------------------------------------------
%% @param   Level the log level
%% @param   Module the module
%% @returns `true' if logging should be permitted at the specified level for the
%%          specified module; `false', otherwise.
%% @doc     Determine whether logging should be permitted at the specified level for the
%%          specified module
%% @end
%%-----------------------------------------------------------------------------
-spec allow(Level :: level(), Module :: atom()) -> boolean().
allow(Level, Module) ->
    ValidLevel = validate_level(Level),
    logger_manager:allow(ValidLevel, Module).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at emergency log level.
%% @end
%%-----------------------------------------------------------------------------
-spec emergency(StringOrReport :: string_or_report()) -> ok.
emergency(StringOrReport) ->
    log(emergency, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at emergency log level.
%% @end
%%-----------------------------------------------------------------------------
-spec emergency(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
emergency(FormatOrReport, ArgsOrMeta) ->
    log(emergency, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at emergency log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec emergency(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
emergency(Format, Args, MetaData) ->
    log(emergency, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at alert log level.
%% @end
%%-----------------------------------------------------------------------------
-spec alert(StringOrReport :: string_or_report()) -> ok.
alert(StringOrReport) ->
    log(alert, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at alert log level.
%% @end
%%-----------------------------------------------------------------------------
-spec alert(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
alert(FormatOrReport, ArgsOrMeta) ->
    log(alert, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at alert log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec alert(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
alert(Format, Args, MetaData) ->
    log(alert, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at critical log level.
%% @end
%%-----------------------------------------------------------------------------
-spec critical(StringOrReport :: string_or_report()) -> ok.
critical(StringOrReport) ->
    log(critical, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at critical log level.
%% @end
%%-----------------------------------------------------------------------------
-spec critical(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
critical(FormatOrReport, ArgsOrMeta) ->
    log(critical, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at critical log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec critical(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
critical(Format, Args, MetaData) ->
    log(critical, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at error log level.
%% @end
%%-----------------------------------------------------------------------------
-spec error(StringOrReport :: string_or_report()) -> ok.
error(StringOrReport) ->
    log(error, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at error log level.
%% @end
%%-----------------------------------------------------------------------------
-spec error(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
error(FormatOrReport, ArgsOrMeta) ->
    log(error, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at error log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec error(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
error(Format, Args, MetaData) ->
    log(error, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at warning log level.
%% @end
%%-----------------------------------------------------------------------------
-spec warning(StringOrReport :: string_or_report()) -> ok.
warning(StringOrReport) ->
    log(warning, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at warning log level.
%% @end
%%-----------------------------------------------------------------------------
-spec warning(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
warning(FormatOrReport, ArgsOrMeta) ->
    log(warning, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at warning log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec warning(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
warning(Format, Args, MetaData) ->
    log(warning, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at notice log level.
%% @end
%%-----------------------------------------------------------------------------
-spec notice(StringOrReport :: string_or_report()) -> ok.
notice(StringOrReport) ->
    log(notice, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at notice log level.
%% @end
%%-----------------------------------------------------------------------------
-spec notice(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
notice(FormatOrReport, ArgsOrMeta) ->
    log(notice, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at notice log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec notice(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
notice(Format, Args, MetaData) ->
    log(notice, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at info log level.
%% @end
%%-----------------------------------------------------------------------------
-spec info(StringOrReport :: string_or_report()) -> ok.
info(StringOrReport) ->
    log(info, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at info log level.
%% @end
%%-----------------------------------------------------------------------------
-spec info(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
info(FormatOrReport, ArgsOrMeta) ->
    log(info, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at info log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec info(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
info(Format, Args, MetaData) ->
    log(info, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   StringOrReport string or report
%% @returns `ok'
%% @doc     Log a string at debug log level.
%% @end
%%-----------------------------------------------------------------------------
-spec debug(StringOrReport :: string_or_report()) -> ok.
debug(StringOrReport) ->
    log(debug, StringOrReport).

%%-----------------------------------------------------------------------------
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at debug log level.
%% @end
%%-----------------------------------------------------------------------------
-spec debug(FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) -> ok.
debug(FormatOrReport, ArgsOrMeta) ->
    log(debug, FormatOrReport, ArgsOrMeta).

%%-----------------------------------------------------------------------------
%% @param   Format format string
%% @param   Args format string arguments
%% @param   MetaData log metadata
%% @returns `ok'
%% @doc     Log a format string with args at debug log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec debug(Format :: string(), Args :: list(), MetaData :: map()) -> ok.
debug(Format, Args, MetaData) ->
    log(debug, Format, Args, MetaData).

%%-----------------------------------------------------------------------------
%% @param   Level log level
%% @param   StringOrReport string or report map
%% @returns `ok'
%% @doc     Log a string at the specified log level.
%% @end
%%-----------------------------------------------------------------------------
-spec log(Level :: level(), StringOrReport :: string_or_report()) -> ok.
log(Level, StringOrReport) ->
    maybe_log(Level, StringOrReport, [], #{}).

%%-----------------------------------------------------------------------------
%% @param   Level log level
%% @param   FormatOrReport format string or report
%% @param   ArgsOrMeta format string arguments or metadata
%% @returns `ok'
%% @doc     Log a format string with args at the specified log level.
%% @end
%%-----------------------------------------------------------------------------
-spec log(Level :: level(), FormatOrReport :: string_or_report(), ArgsOrMeta :: list() | map()) ->
    ok.
log(Level, FormatOrReport, ArgsOrMeta) when is_map(FormatOrReport) andalso is_map(ArgsOrMeta) ->
    maybe_log(Level, FormatOrReport, [], ArgsOrMeta);
log(Level, FormatOrReport, ArgsOrMeta) when is_list(FormatOrReport) andalso is_list(ArgsOrMeta) ->
    maybe_log(Level, FormatOrReport, ArgsOrMeta, #{}).

%%-----------------------------------------------------------------------------
%% @param   Level log level
%% @param   Format format string
%% @param   Args format string arguments
%% @param   Meta log metadata
%% @returns `ok'
%% @doc     Log a format string with args at the specified log level with
%%          the specified metadata.
%% @end
%%-----------------------------------------------------------------------------
-spec log(Level :: level(), Format :: string(), Args :: list(), Meta :: map()) -> ok.
log(Level, Format, Args, Meta) ->
    maybe_log(Level, Format, Args, Meta).

%%-----------------------------------------------------------------------------
%% @param   Level1 a level
%% @param   Level2 a level
%% @returns `lt | eq | gt'
%% @doc     Return comparison between levels
%%
%%          `lt' if `Level1 < Level2'
%%          `eq' if `Level1 == Level2'
%%          `gt' if `Level1 > Level2'
%% @end
%%-----------------------------------------------------------------------------
-spec compare(Level1 :: level(), Level2 :: level()) -> lt | eq | gt.
compare(Level1, Level2) ->
    compare_int_level(to_int_level(Level1), to_int_level(Level2)).

%%
%% Note.  macro_log is designed to be called only from logging
%% macros, so some checks can be skipped, because they
%% will have already been called and the inputs have been
%% validated.
%%

%% @hidden
macro_log(Location, Level, StringOrReport) ->
    do_log(Level, StringOrReport, [], #{location => Location}).

%% @hidden
macro_log(Location, Level, FormatOrReport, ArgsOrMeta) when
    is_map(FormatOrReport) andalso is_map(ArgsOrMeta)
->
    do_log(Level, FormatOrReport, [], ArgsOrMeta#{location => Location});
macro_log(Location, Level, FormatOrReport, ArgsOrMeta) when
    is_list(FormatOrReport) andalso is_list(ArgsOrMeta)
->
    do_log(Level, FormatOrReport, ArgsOrMeta, #{location => Location}).

%% @hidden
macro_log(Location, Level, Format, Args, Meta) ->
    do_log(Level, Format, Args, Meta#{location => Location}).

%%
%% Internal operations
%%

%% @private
maybe_log(Level0, Format, Args, MetaData) ->
    Level = validate_level(Level0),
    case logger_manager:allow(Level, maybe_get_module(MetaData)) of
        true ->
            do_log(Level, Format, Args, MetaData);
        _ ->
            ok
    end.

get_handlers() ->
    %%
    %% TODO We could really use ETS here.  In its absence, because we
    %% are punting on allowing the logger_manager to be mutable, we can
    %% cache the handlers in the process dictionary.  But we need to
    %% keep track of the logger manager internal id, in case it changes
    %% (unlikely, in most scenarios).  This has a small cost of requesting
    %% the id (a reference) from the logger manager on every allowed log call,
    %% even after caching locally in the process dictionary.
    %%
    case erlang:get('$atomvm_logger_handlers') of
        undefined ->
            {_Id, Handlers} = Result = logger_manager:get_handlers(),
            erlang:put('$atomvm_logger_handlers', Result),
            Handlers;
        {Id, Handlers} ->
            case logger_manager:get_id() of
                CurrentId when Id == CurrentId ->
                    Handlers;
                NewId ->
                    {NewId, NewHandlers} = NewResult = logger_manager:get_handlers(),
                    erlang:put('$atomvm_logger_handlers', NewResult),
                    NewHandlers
            end
    end.

%% @private
allow_handler(_Level, all) ->
    true;
allow_handler(_Level, none) ->
    false;
allow_handler(Level, HandlerLevel) ->
    Cmp = ?MODULE:compare(Level, HandlerLevel),
    Cmp == lt orelse Cmp == eq.

%% @private
do_log(Level, StringOrReport, Args, MetaData) when
    (is_list(StringOrReport) orelse is_map(StringOrReport)) andalso is_list(Args) andalso
        is_map(MetaData)
->
    LogEvent = create_event(Level, StringOrReport, Args, MetaData),
    lists:foreach(
        fun({Handler, HandlerConfig}) ->
            case allow_handler(Level, maps:get(level, HandlerConfig)) of
                true ->
                    try
                        Handler:log(LogEvent, HandlerConfig),
                        ok
                    catch
                        T:E:S ->
                            io:format(
                                "An error occurred logging event ~p with config ~p.  type: ~p error=~p stacktrace=~p~n",
                                [LogEvent, HandlerConfig, T, E, S]
                            ),
                            error
                    end;
                _ ->
                    ok
            end
        end,
        get_handlers()
    );
do_log(_Level, _StringOrReport, _Args, _MetaData) ->
    erlang:error(badarg).

%% @private
maybe_get_module(#{location := #{mfa := {Module, _FunctionName, _FunctionArity}}} = _MetaData) ->
    Module;
maybe_get_module(_MetaData) ->
    undefined.

%% @private
create_event(Level, StringOrReport, Args, MetaData) ->
    Msg =
        case is_map(StringOrReport) of
            true ->
                {report, StringOrReport};
            _ ->
                {StringOrReport, Args}
        end,
    #{
        level => Level,
        msg => Msg,
        meta => MetaData,
        pid => self(),
        timestamp => erlang:system_time(microsecond)
    }.

%% @private
validate_level(Level) when
    Level == emergency orelse
        Level == alert orelse
        Level == critical orelse
        Level == error orelse
        Level == warning orelse
        Level == notice orelse
        Level == info orelse
        Level == debug
->
    Level.

%% @private
to_int_level(emergency) ->
    0;
to_int_level(alert) ->
    1;
to_int_level(critical) ->
    2;
to_int_level(error) ->
    3;
to_int_level(warning) ->
    4;
to_int_level(notice) ->
    5;
to_int_level(info) ->
    6;
to_int_level(debug) ->
    7.

%% @private
compare_int_level(Level, Level) ->
    eq;
compare_int_level(Level1, Level2) when Level1 < Level2 ->
    lt;
compare_int_level(_Level1, _Level2) ->
    gt.
