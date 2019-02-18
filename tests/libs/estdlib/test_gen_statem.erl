-module(test_gen_statem).

-export([test/0]).
-export([init/1, initial/3, terminate/3]).

-include("estdlib.hrl").

-record(data, {
    num_casts=0,
    num_infos=0
}).

test() ->
    ok = test_call(),
    ok = test_cast(),
    ok = test_info(),
    ok.


test_call() ->
    {ok, Pid} = ?GEN_STATEM:start(?MODULE, [], []),
    pong = ?GEN_STATEM:call(Pid, ping),
    ?GEN_STATEM:stop(Pid),
    ok.

test_cast() ->
    {ok, Pid} = ?GEN_STATEM:start(?MODULE, [], []),

    ok = ?GEN_STATEM:cast(Pid, ping),
    ok = ?GEN_STATEM:cast(Pid, ping),
    ok = ?GEN_STATEM:cast(Pid, ping),
    ok = ?GEN_STATEM:cast(Pid, ping),
    ok = ?GEN_STATEM:cast(Pid, ping),

    5 = ?GEN_STATEM:call(Pid, get_num_casts),
    0 = ?GEN_STATEM:call(Pid, get_num_casts),

    ?GEN_STATEM:stop(Pid),
    ok.

test_info() ->
    {ok, Pid} = ?GEN_STATEM:start(?MODULE, [], []),

    Pid ! ping,
    Pid ! ping,
    Pid ! ping,

    3 = ?GEN_STATEM:call(Pid, get_num_infos),
    0 = ?GEN_STATEM:call(Pid, get_num_infos),

    ?GEN_STATEM:stop(Pid),
    ok.

%%
%% callbacks
%%

init(_) ->
    {ok, initial, #data{}}.

initial({call, From}, ping, Data) ->
    {next_state, initial, Data, [{reply, From, pong}]};
initial(cast, ping, #data{num_casts=NumCasts} = Data) ->
    {next_state, initial, Data#data{num_casts=NumCasts+1}};
initial(info, ping, #data{num_infos=NumInfos} = Data) ->
    {next_state, initial, Data#data{num_infos=NumInfos+1}};
initial({call, From}, get_num_casts, #data{num_casts=NumCasts} = Data) ->
    {next_state, initial, Data#data{num_casts=0}, [{reply, From, NumCasts}]};
initial({call, From}, get_num_infos, #data{num_infos=NumInfos} = Data) ->
    {next_state, initial, Data#data{num_infos=0}, [{reply, From, NumInfos}]}.

terminate(_Reason, _StateName, _Data) ->
    ok.
