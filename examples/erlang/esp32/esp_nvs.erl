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
                    case esp:random() rem 100 of
                        0 ->
                            erlang:display(reformat),
                            esp:nvs_reformat(),
                            #state{};
                        1 ->
                            erlang:display(erase_all),
                            esp:nvs_erase_all(?MODULE),
                            #state{};
                        2 ->
                            erlang:display(erase_key),
                            esp:nvs_erase_key(?MODULE, start),
                            #state{};
                        _ ->
                            #state{count = Count + 1}
                    end;
                _ ->
                    erlang:display({error, bad_value}),
                    #state{}
            end
    end,
    erlang:display(State),
    esp:nvs_set_binary(?MODULE, starts, erlang:term_to_binary(State)).
