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

-module(test_logger).

-export([test/0, log/2]).

-include_lib("kernel/include/logger.hrl").
-include("etest.hrl").

test() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            ok = test_logger();
        _ ->
            ok
    end.

test_logger() ->
    ok = test_default_logger(),
    ok = test_log_handler(),
    ok = test_primary_log_level(),
    ok = test_module_log_level(),
    ok = test_disable_default_logger(),
    ok = test_replace_default_log_handler(),
    ok = test_log_handler_log_level(),
    ok = test_format(),
    ok = test_report(),
    ok.

test_default_logger() ->
    %% silent "failure" (nothing will be logged) if the
    %% logger manager has not been instantiated.  In OTP-style
    %% deployment, the logger manager is initialized in
    %% the kernel application.
    ok = ?LOG_NOTICE("Tried to log before starting log_manager!"),

    {ok, _Pid} = logger_manager:start_link(#{}),

    ok = ?LOG_EMERGENCY("Emergency!"),
    ok = ?LOG_ALERT("Alert!"),
    ok = ?LOG_CRITICAL("Critical!"),
    ok = ?LOG_ERROR("Error!"),
    ok = ?LOG_WARNING("Warning!"),
    ok = ?LOG_NOTICE("Notice!"),
    ok = ?LOG_INFO("Info!"),
    ok = ?LOG_DEBUG("Debug!"),

    ?ASSERT_ERROR(logger:log(bad_level, "foo")),
    ?ASSERT_ERROR(logger:log(notice, not_a_list)),
    ?ASSERT_ERROR(logger:log(notice, "", not_a_list)),
    ?ASSERT_ERROR(logger:log(notice, "", [], not_a_map)),

    logger_manager:stop(),

    ok.

test_log_handler() ->
    {ok, _Pid} = logger_manager:start_link(#{
        logger => [
            {handler, test_handler, ?MODULE, #{config => #{log_receiver => self()}}}
        ]
    }),

    ok = ?LOG_NOTICE("test_log_handler!"),
    Event = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event), notice),
    ?ASSERT_MATCH(maps:get(msg, Event), {"test_log_handler!", []}),
    ?ASSERT_MATCH(
        maps:get(mfa, maps:get(location, maps:get(meta, Event))),
        {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}
    ),

    %% no location if logging macro is not used
    ok = logger:notice("test_log_handler!"),
    Event2 = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event2), notice),
    ?ASSERT_MATCH(maps:get(msg, Event2), {"test_log_handler!", []}),
    ?ASSERT_TRUE(not maps:is_key(location, maps:get(meta, Event2))),

    ok = ?LOG_INFO("test_log_handler!"),
    timeout = pop_event(),

    logger_manager:stop(),

    ok.

test_primary_log_level() ->
    {ok, _Pid} = logger_manager:start_link(#{
        log_level => emergency,
        logger => [
            {handler, test_handler, ?MODULE, #{config => #{log_receiver => self()}}}
        ]
    }),

    ok = ?LOG_EMERGENCY("test_primary_log_level!"),
    Event = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event), emergency),
    ?ASSERT_MATCH(maps:get(msg, Event), {"test_primary_log_level!", []}),
    ?ASSERT_MATCH(
        maps:get(mfa, maps:get(location, maps:get(meta, Event))),
        {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}
    ),

    ok = ?LOG_NOTICE("test_primary_log_level!"),
    timeout = pop_event(),

    logger_manager:stop(),

    ok.

test_module_log_level() ->
    {ok, _Pid} = logger_manager:start_link(#{
        logger => [
            {handler, test_handler, ?MODULE, #{config => #{log_receiver => self()}}},
            {module_level, info, [?MODULE, some_other_module_we_dont_care_about]}
        ]
    }),

    ok = ?LOG_INFO("test_module_log_level!"),
    Event = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event), info),
    ?ASSERT_MATCH(maps:get(msg, Event), {"test_module_log_level!", []}),
    ?ASSERT_MATCH(
        maps:get(mfa, maps:get(location, maps:get(meta, Event))),
        {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}
    ),

    %% without macro-log the module is not known, so should default to not logging
    ok = logger:log(info, "test_module_log_level!"),
    timeout = pop_event(),

    ok = ?LOG_DEBUG("test_module_log_level!"),
    timeout = pop_event(),

    logger_manager:stop(),

    ok.

test_disable_default_logger() ->
    {ok, _Pid} = logger_manager:start_link(#{
        logger => [
            {handler, default, undefined}
        ]
    }),

    ok = ?LOG_EMERGENCY("test_disable_default_logger!"),
    timeout = pop_event(),
    ok = ?LOG_DEBUG("test_disable_default_logger!"),
    timeout = pop_event(),

    logger_manager:stop(),

    ok.

test_replace_default_log_handler() ->
    {ok, _Pid} = logger_manager:start_link(#{
        logger => [
            {handler, default, ?MODULE, #{config => #{log_receiver => self()}}}
        ]
    }),

    ok = ?LOG_NOTICE("test_replace_default_log_handler!"),
    Event = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event), notice),
    ?ASSERT_MATCH(maps:get(msg, Event), {"test_replace_default_log_handler!", []}),
    ?ASSERT_MATCH(
        maps:get(mfa, maps:get(location, maps:get(meta, Event))),
        {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}
    ),

    ok = ?LOG_INFO("test_replace_default_log_handler!"),
    timeout = pop_event(),

    logger_manager:stop(),

    ok.

test_log_handler_log_level() ->
    {ok, _Pid} = logger_manager:start_link(#{
        logger => [
            {handler, default, ?MODULE, #{config => #{log_receiver => self()}}},
            {handler, test_handler, ?MODULE, #{
                config => #{log_receiver => self()}, level => emergency
            }}
        ]
    }),

    ok = ?LOG_NOTICE("test_log_handler_log_level!"),
    Event = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event), notice),
    ?ASSERT_MATCH(maps:get(msg, Event), {"test_log_handler_log_level!", []}),
    ?ASSERT_MATCH(
        maps:get(mfa, maps:get(location, maps:get(meta, Event))),
        {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}
    ),

    %% second handler should not have triggered because it only logs at emergency
    timeout = pop_event(),

    logger_manager:stop(),

    ok.

test_format() ->
    {ok, _Pid} = logger_manager:start_link(#{
        logger => [
            {handler, test_handler, ?MODULE, #{
                config => #{log_receiver => self()}
            }}
        ]
    }),

    ok = ?LOG_NOTICE("foo ~p", [bar]),
    Event = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event), notice),
    ?ASSERT_MATCH(maps:get(msg, Event), {"foo ~p", [bar]}),

    Meta = #{gnu => gnat},
    ok = ?LOG_NOTICE("bar ~p", [tapas], Meta),
    Event2 = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event2), notice),
    ?ASSERT_MATCH(maps:get(msg, Event2), {"bar ~p", [tapas]}),
    ?ASSERT_MATCH(maps:get(gnu, maps:get(meta, Event2)), gnat),

    logger_manager:stop(),

    ok.

test_report() ->
    {ok, _Pid} = logger_manager:start_link(#{
        logger => [
            {handler, test_handler, ?MODULE, #{
                config => #{log_receiver => self()}
            }}
        ]
    }),

    Report = #{foo => bar, bar => tapas},
    ok = ?LOG_NOTICE(Report),
    Event = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event), notice),
    ?ASSERT_MATCH(maps:get(msg, Event), {report, Report}),

    Meta = #{gnu => gnat},
    ok = ?LOG_NOTICE(Report, Meta),
    Event2 = pop_event(),
    ?ASSERT_MATCH(maps:get(level, Event2), notice),
    ?ASSERT_MATCH(maps:get(msg, Event2), {report, Report}),
    ?ASSERT_MATCH(maps:get(gnu, maps:get(meta, Event2)), gnat),

    logger_manager:stop(),

    ok.

%% logger callback function
log(LogEvent, #{config := #{log_receiver := LogReceiver}}) ->
    LogReceiver ! {log, self(), LogEvent},
    ok.

pop_event() ->
    receive
        {log, _Pid, Event} ->
            Event
    after 100 ->
        timeout
    end.
