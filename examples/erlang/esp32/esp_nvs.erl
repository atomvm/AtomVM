-module(esp_nvs).
-export([start/0]).

-record(
    record, {
        count = 0,
        timestamp = erlang:timestamp()
    }
).

start() ->
    Bin = esp:nvs_get_binary(start),
    Record = case Bin of
        undefined -> 
            #record{};
        _ -> 
            case erlang:binary_to_term(Bin) of
                #record{count=Count} ->
                    case esp:random() rem 100 of
                        0 ->
                            erlang:display(reformat),
                            esp:nvs_reformat(),
                            #record{};
                        50 ->
                            erlang:display(erase_all),
                            esp:nvs_erase_all(),
                            #record{};
                        99 ->
                            erlang:display(erase_key),
                            esp:nvs_erase_key(start),
                            #record{};
                        _ ->
                            #record{count = Count + 1}
                    end;
                _ ->
                    erlang:display({error, bad_value}),
                    #record{}
            end
    end,
    erlang:display({start, Record}),
    esp:nvs_set_binary(start, erlang:term_to_binary(Record)).
