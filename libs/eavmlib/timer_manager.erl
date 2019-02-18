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
%% @hidden
%%-----------------------------------------------------------------------------
-module(timer_manager).

-export([start/0, start_timer/3, cancel_timer/1, get_timer_refs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([run_timer/5]).

-include("estdlib.hrl").

-record(state, {
    timers = [] :: [{reference(), pid()}]
}).

-include("logger.hrl").

-define(SERVER_NAME, ?MODULE).

-spec start() -> {ok, Pid::pid()} | {error, Reason::term()}.
start() ->
    ?LOG_DEBUG(start),
    ?GEN_SERVER:start({local, ?SERVER_NAME}, ?MODULE, [], []).

-spec start_timer(Time::non_neg_integer(), Dest::pid(), Msg::term()) -> TimerRef::reference().
start_timer(Time, Dest, Msg) ->
    ?LOG_DEBUG({start_timer, Time, Dest, Msg}),
    ?GEN_SERVER:call(?SERVER_NAME, {Time, Dest, Msg}).

-spec cancel_timer(TimerRef::reference()) -> ok.
cancel_timer(TimerRef) ->
    ?LOG_DEBUG({cancel_timer, TimerRef}),
    ?GEN_SERVER:call(?SERVER_NAME, {cancel, TimerRef}).

-spec get_timer_refs() -> [reference()].
get_timer_refs() ->
    ?LOG_DEBUG(get_timer_refs),
    ?GEN_SERVER:call(?SERVER_NAME, get_timer_refs).

%%
%% ?GEN_SERVER callbacks
%%

%% @hidden
init([]) ->
    {ok, #state{}}.

%% @hidden
handle_call(get_timer_refs, _From, #state{timers=Timers} = State) ->
    {reply, [TimerRef || {TimerRef, _Pid} <- Timers], State};
handle_call({cancel, TimerRef}, From, #state{timers=Timers} = State) ->
    case ?LISTS:keyfind(TimerRef, 1, Timers) of
        false ->
            {reply, false, State};
        {TimerRef, Pid} ->
            Pid ! {cancel, From},
            {noreply, State}
    end;
handle_call({Time, Dest, Msg}, _From, #state{timers=Timers} = State) ->
    {TimerRef, Pid} = do_start_timer(Time, Dest, Msg),
    {reply, TimerRef, State#state{timers=[{TimerRef, Pid} | Timers]}}.

%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.

%% @hidden
handle_info({finished, TimerRef}, #state{timers=Timers} = State) ->
    {noreply, State#state{timers=?LISTS:keydelete(TimerRef, 1, Timers)}}.

%% @hidden
terminate(_Reason, _State) ->
    ok.


%%=============================================================================
%% internal functions

%% @private
do_start_timer(Time, Dest, Msg) ->
    TimerRef = erlang:make_ref(),
    Pid = spawn(?MODULE, run_timer, [self(), Time, TimerRef, Dest, Msg]),
    {TimerRef, Pid}.

%% @private
run_timer(MgrPid, Time, TimerRef, Dest, Msg) ->
    Start = erlang:timestamp(),
    receive
        {cancel, From} ->
            ?GEN_SERVER:reply(From, Time - timestamp_util:delta_ms(erlang:timestamp(), Start))
    after Time ->
        Dest ! {timeout, TimerRef, Msg}
    end,
    MgrPid ! {finished, TimerRef}.
