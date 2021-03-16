-module(ping_pong_server).

-behavior(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2]).

start_link(Parent) ->
    gen_server:start_link(?MODULE, [Parent], []).

init([Parent]) ->
    Parent ! {ping_pong_server_ready, self()},
    {ok, nil}.

handle_call(exit, From, State) ->
    gen_server:reply(From, ok),
    {stop, exit, noreply, State};
handle_call(ping, From, State) ->
    gen_server:reply(From, pong),
    {noreply, State}.

handle_cast({crash, Reason}, _State) ->
    throw(Reason).
