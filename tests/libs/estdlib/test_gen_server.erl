-module(test_gen_server).

-export([test/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("estdlib.hrl").

-record(state, {
    num_casts=0,
    num_infos=0
}).

test() ->
    ok = test_call(),
    ok = test_cast(),
    ok = test_info(),
    ok.

test_call() ->
    {ok, Pid} = ?GEN_SERVER:start(?MODULE, [], []),

    pong = ?GEN_SERVER:call(Pid, ping),
    pong = ?GEN_SERVER:call(Pid, reply_ping),
    %pong = ?GEN_SERVER:call(Pid, async_ping),

    ?GEN_SERVER:stop(Pid),
    ok.

test_cast() ->
    {ok, Pid} = ?GEN_SERVER:start(?MODULE, [], []),

    ok = ?GEN_SERVER:cast(Pid, ping),
    ok = ?GEN_SERVER:cast(Pid, ping),
    ok = ?GEN_SERVER:cast(Pid, ping),
    ok = ?GEN_SERVER:cast(Pid, ping),
    ok = ?GEN_SERVER:cast(Pid, ping),

    5 = ?GEN_SERVER:call(Pid, get_num_casts),
    0 = ?GEN_SERVER:call(Pid, get_num_casts),

    ?GEN_SERVER:stop(Pid),
    ok.

test_info() ->
    {ok, Pid} = ?GEN_SERVER:start(?MODULE, [], []),

    Pid ! ping,
    Pid ! ping,
    Pid ! ping,

    3 = ?GEN_SERVER:call(Pid, get_num_infos),
    0 = ?GEN_SERVER:call(Pid, get_num_infos),

    ?GEN_SERVER:stop(Pid),
    ok.


%%
%% callbacks
%%

init(_) ->
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(reply_ping, From, State) ->
    ?GEN_SERVER:reply(From, pong),
    {noreply, State};
handle_call(async_ping, From, State) ->
    erlang:spawn(gen_server, reply, [{From, pong}]),
    {noreply, State};
handle_call(get_num_casts, From, #state{num_casts=NumCasts} = State) ->
    ?GEN_SERVER:reply(From, NumCasts),
    {noreply, State#state{num_casts=0}};
handle_call(get_num_infos, From, #state{num_infos=NumInfos} = State) ->
    ?GEN_SERVER:reply(From, NumInfos),
    {noreply, State#state{num_infos=0}}.

handle_cast(ping, #state{num_casts=NumCasts} = State) ->
    {noreply, State#state{num_casts=NumCasts + 1}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(ping, #state{num_infos=NumInfos} = State) ->
    {noreply, State#state{num_infos=NumInfos + 1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
