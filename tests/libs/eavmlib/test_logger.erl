-module(test_logger).

-export([test/0, do_log/1, counter/1]).

-include("estdlib.hrl").
-include("etest.hrl").
-include("logger.hrl").

test() ->
    start_counter(),
    logger:start([{sink, {?MODULE, do_log}}]),

    ?ASSERT_MATCH(get_counter(info), 0),
    ?ASSERT_MATCH(get_counter(warning), 0),
    ?ASSERT_MATCH(get_counter(error), 0),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_INFO(ok),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 0),
    ?ASSERT_MATCH(get_counter(error), 0),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_WARNING(ok),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 0),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_ERROR(ok),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 1),
    ?ASSERT_MATCH(get_counter(debug), 0),

    ok = ?LOG_DEBUG(ok),
    ?ASSERT_MATCH(get_counter(info), 1),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 1),
    ?ASSERT_MATCH(get_counter(debug), 0),

    logger:set_levels([debug, info]),
    ok = ?LOG_INFO(ok),
    ok = ?LOG_WARNING(ok),
    ok = ?LOG_ERROR(ok),
    ok = ?LOG_DEBUG(ok),
    ?ASSERT_MATCH(get_counter(info), 2),
    ?ASSERT_MATCH(get_counter(warning), 1),
    ?ASSERT_MATCH(get_counter(error), 1),
    ?ASSERT_MATCH(get_counter(debug), 1),

    ok.


do_log({_Location, _Time, _Pid, Level, _Msg} = _LogRequest) ->
    increment_counter(Level).


-record(state, {
    counters = [
        {info, 0},
        {warning, 0},
        {error, 0},
        {debug, 0}
    ]
}).

start_counter() ->
    Pid = spawn(?MODULE, counter, [#state{}]),
    erlang:register(counter, Pid).

increment_counter(Level) ->
    Pid = erlang:whereis(counter),
    Pid ! {increment, Level}.

get_counter(Level) ->
    Pid = erlang:whereis(counter),
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, get_counter, Level},
    receive
        {Ref, Counter} -> Counter
    end.

counter(#state{counters=Counters} = State) ->
    NewState = receive
        {increment, Level} ->
            Value = ?PROPLISTS:get_value(Level, Counters),
            State#state{counters=[{Level, Value + 1} | ?LISTS:keydelete(Level, 1, Counters)]};
        {Pid, Ref, get_counter, Level} ->
            Pid ! {Ref, ?PROPLISTS:get_value(Level, Counters)},
            State
    end,
    counter(NewState).
