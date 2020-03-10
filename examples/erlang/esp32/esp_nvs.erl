-module(esp_nvs).
-export([start/0]).

-record(
    state, {
        count = 0,
        timestamp = erlang:timestamp()
    }
).

start() ->
    Bin = esp:nvs_get_binary(?MODULE, starts),
    State = case Bin of
        undefined ->
            #state{};
        _ ->
            case erlang:binary_to_term(Bin) of
                #state{count=Count} ->
                    #state{count = Count + 1};
                _ ->
                    erlang:display({error, bad_value}),
                    #state{}
            end
    end,
    io:format("Saving count ~p to NVS...~nReset device to increment.~n", [State#state.count]),
    esp:nvs_set_binary(?MODULE, starts, erlang:term_to_binary(State)).
