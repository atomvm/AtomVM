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
-module(logger_manager).

-export([
    start_link/1,
    stop/0,
    get_handlers/0,
    get_id/0,
    allow/2
]).

-behavior(gen_server).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-record(state, {
    id = erlang:make_ref(),
    log_level,
    default_handler,
    handlers,
    module_levels
}).

-type handler_id() :: atom().
-type handler_config() :: #{
    id => atom(),
    module => module(),
    level => logger:level(),
    config => term()
}.
-type logger_config() :: [
    {handler, default, undefined}
    | {handler, HandlerId :: handler_id(), Handler :: module(), HandlerConfig :: handler_config()}
    | {module_level, logger:level(), [module()]}
].
-type config() :: #{
    log_level => logger:level(),
    logger => logger_config()
}.

-define(DEFAULT_LOGGER, {handler, default, logger_std_h, #{}}).
-define(DEFAULT_LOG_LEVEL, notice).

%% @hidden
-spec start_link(Config :: config()) -> {ok, pid()} | {error, Reason :: term()}.
start_link(Config) ->
    gen_server:start({local, ?MODULE}, ?MODULE, normalize_config(Config), []).

%% @hidden
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @hidden
get_handlers() ->
    gen_server:call(?MODULE, get_handlers).

%% @hidden
get_id() ->
    gen_server:call(?MODULE, get_id).

%% @hidden
allow(Level, Module) ->
    %% if the logger manager has not been instantiated, then
    %% "fail" silently (i.e., log nothing).  The logger manager
    %% either needs to be instantiated manually, or it is be done
    %% so automatically in OTP-style deployment when the kernel
    %% is started by init.
    case erlang:whereis(?MODULE) of
        undefined ->
            false;
        _ ->
            gen_server:call(?MODULE, {allow, Level, Module})
    end.

%%
%% gen_server callbacks
%%

%% @hidden
init(Config) ->
    {DefaultHandler, Handlers, ModuleLevels} = parse_logger_config(
        maps:get(logger, Config), {undefined, #{}, #{}}, true
    ),
    {ok, #state{
        log_level = maps:get(log_level, Config, ?DEFAULT_LOG_LEVEL),
        default_handler = DefaultHandler,
        handlers = Handlers,
        module_levels = ModuleLevels
    }}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_call({allow, Level, Module}, _From, State) ->
    {reply, do_allow(Level, Module, State), State};
handle_call(get_handlers, _From, State) ->
    {reply, {State#state.id, do_get_handlers(State)}, State};
handle_call(get_id, _From, State) ->
    {reply, State#state.id, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unimplemented}, State}.

%% @hidden
handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%%
%% internal implementation
%%

%% @private
get_log_level(#{log_level := LogLevel}) ->
    LogLevel;
get_log_level(_Config) ->
    ?DEFAULT_LOG_LEVEL.

%% @private
is_default_logger({handler, default, _Handler, _HandlerConfig}) ->
    true;
is_default_logger(_) ->
    false.

%% @private
has_default_log_handler(Logger) ->
    lists:any(
        fun is_default_logger/1,
        Logger
    ).

%% @private
get_logger(#{logger := Logger}) ->
    case lists:member({handler, default, undefined}, Logger) of
        true ->
            [
                Entry
             || {handler, HandlerId, _Module, _ModuleConfig} = Entry <- Logger,
                HandlerId =/= default
            ];
        _ ->
            case has_default_log_handler(Logger) of
                true ->
                    Logger;
                _ ->
                    [?DEFAULT_LOGGER] ++ Logger
            end
    end;
get_logger(_Config) ->
    [?DEFAULT_LOGGER].

%% @private
normalize_config(Config) ->
    #{
        log_level => get_log_level(Config),
        logger => get_logger(Config)
    }.

%% private
maybe_get_level(#{level := Level}) ->
    Level;
maybe_get_level(_HandlerConfig) ->
    all.

%% private
fill_handler_config(HandlerId, Module, HandlerConfig) when is_map(HandlerConfig) ->
    HandlerConfig#{
        id => HandlerId,
        module => Module,
        level => maybe_get_level(HandlerConfig)
    }.

%% @private
parse_logger_config([], Accum, _AllowsDefaultHandler) ->
    Accum;
parse_logger_config(
    [{handler, default, undefined} | Rest],
    {undefined, _Handlers, _ModuleLevels} = Accum,
    _AllowsDefaultHandler
) ->
    parse_logger_config(Rest, Accum, false);
parse_logger_config([{handler, default, undefined} | _Rest], _Accum, false) ->
    %% we already have a default handler but it is disabled, this is an error
    error(badarg);
parse_logger_config([{handler, default, _Module, _HandlerConfig} | _Rest], _Accum, false) ->
    %% we can't have any default handler if it has been disabled
    error(badarg);
parse_logger_config(
    [{handler, default, _Module, _HandlerConfig} | _Rest],
    {{_DefaultHandlerModule, _DefaultHandlerConfig}, _Handlers, _ModuleLevels} = _Accum,
    _AllowsDefaultHandler
) ->
    %% we can't have more than one default handler if it has already been defined
    error(badarg);
parse_logger_config(
    [{handler, default, Module, HandlerConfig} | Rest],
    {undefined, Handlers, ModuleLevels} = _Accum,
    true
) ->
    %% we have a default handler
    parse_logger_config(
        Rest,
        {{Module, fill_handler_config(default, Module, HandlerConfig)}, Handlers, ModuleLevels},
        true
    );
parse_logger_config(
    [{handler, HandlerId, Module, HandlerConfig} | Rest],
    {DefaultHandler, Handlers, ModuleLevels} = _Accum,
    AllowsDefaultHandler
) ->
    %% we have a non-default handler
    parse_logger_config(
        Rest,
        {DefaultHandler,
            Handlers#{HandlerId => {Module, fill_handler_config(HandlerId, Module, HandlerConfig)}},
            ModuleLevels},
        AllowsDefaultHandler
    );
parse_logger_config(
    [{module_level, Level, Modules} | Rest],
    {DefaultHandler, Handlers, ModuleLevels} = _Accum,
    AllowsDefaultHandler
) ->
    parse_logger_config(
        Rest,
        {DefaultHandler, Handlers, set_module_levels(ModuleLevels, Level, Modules)},
        AllowsDefaultHandler
    );
parse_logger_config([_ | Rest], Accum, AllowsDefaultHandler) ->
    %% skip other config
    parse_logger_config(Rest, Accum, AllowsDefaultHandler).

%% @private
set_module_levels(ModuleLevels, Level, Modules) ->
    lists:foldl(
        fun(Module, Accum) ->
            Accum#{Module => Level}
        end,
        ModuleLevels,
        Modules
    ).

%% @private
do_allow(Level, Module, State) ->
    EffectiveLevel = maps:get(Module, State#state.module_levels, State#state.log_level),
    Cmp = logger:compare(Level, EffectiveLevel),
    Cmp == lt orelse Cmp == eq.

do_get_handlers(State) ->
    Handlers = maps:values(State#state.handlers),
    case State#state.default_handler of
        undefined ->
            Handlers;
        DefaultHandler ->
            [DefaultHandler] ++ Handlers
    end.
