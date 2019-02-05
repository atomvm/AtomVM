%% Adapted from Erlang/OTP gen_statem documentation
-module(code_lock).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start/0, start/1]).
-export([button/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

-include("estdlib.hrl").

-record(state, {
    code, length, buttons
}).


start() ->
    code_lock:start([1,2,3,4]),
    code_lock:button(1),
    code_lock:button(2),
    code_lock:button(3),
    code_lock:button(4),
    timer:sleep(30000),
    ok.

start(Code) ->
    ?GEN_STATEM:start({local,?NAME}, ?MODULE, Code, []).

button(Button) ->
    ?GEN_STATEM:cast(?NAME, {button,Button}).

init(Code) ->
    do_lock(),
    Data = #state{code = Code, length = length(Code), buttons = []},
    {ok, locked, Data}.

callback_mode() ->
    state_functions.

locked(
  cast, {button,Button},
  #state{code = Code, length = Length, buttons = Buttons} = Data) ->
    NewButtons =
        if
            length(Buttons) < Length ->
                Buttons;
            true ->
                tl(Buttons)
        end ++ [Button],
    if
        NewButtons =:= Code -> % Correct
        do_unlock(),
            {next_state, open, Data#state{buttons = []},
             [{state_timeout,10000,lock}]}; % Time in milliseconds
    true -> % Incomplete | Incorrect
            {next_state, locked, Data#state{buttons = NewButtons}}
    end.

open(state_timeout, lock,  Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button,_}, Data) ->
    {next_state, open, Data}.

do_lock() ->
    erlang:display(lock).
do_unlock() ->
    erlang:display(unlock).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
