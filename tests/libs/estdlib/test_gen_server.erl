-module(test_gen_server).

-export([test/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    pong = gen_server:call(Pid, ping),
    pong = gen_server:call(Pid, reply_ping),
    %pong = gen_server:call(Pid, async_ping),

    gen_server:stop(Pid),
    ok.

test_cast() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),
    ok = gen_server:cast(Pid, ping),

    5 = gen_server:call(Pid, get_num_casts),
    0 = gen_server:call(Pid, get_num_casts),

    gen_server:stop(Pid),
    ok.

test_info() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),

    Pid ! ping,
    Pid ! ping,
    Pid ! ping,

    3 = gen_server:call(Pid, get_num_infos),
    0 = gen_server:call(Pid, get_num_infos),

    gen_server:stop(Pid),
    ok.


%%
%% callbacks
%%

init(_) ->
    {ok, #state{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(reply_ping, From, State) ->
    gen_server:reply(From, pong),
    {noreply, State};
handle_call(async_ping, From, State) ->
    erlang:spawn(gen_server, reply, [{From, pong}]),
    {noreply, State};
handle_call(get_num_casts, From, #state{num_casts=NumCasts} = State) ->
    gen_server:reply(From, NumCasts),
    {noreply, State#state{num_casts=0}};
handle_call(get_num_infos, From, #state{num_infos=NumInfos} = State) ->
    gen_server:reply(From, NumInfos),
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
