-module(server).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("estdlib.hrl").

-record(state, {
    %% fun stuff goes here
}).


start() ->
    {ok, Pid} = ?GEN_SERVER:start(?MODULE, [], []),
    Reply = ?GEN_SERVER:call(Pid, hello),
    erlang:display(Reply),
    ok.


init(Args) ->
    console:puts("init: "), erlang:display(Args),
    {ok, #state{}}.

handle_call(hello, _From, State) ->
    {reply, hi, State};
handle_call(Request, _From, State) ->
    {reply, {unknown, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    erlang:display(Info),
    {noreply, State}.

terminate(Reason, _State) ->
    erlang:display(Reason),
    ok.
