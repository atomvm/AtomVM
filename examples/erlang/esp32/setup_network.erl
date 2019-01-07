-module(setup_network).

-export([start/0]).

start() ->
    NetworkConfig = [{sta, [
        {ssid, "mynetwokid"},
        {psk, "mypassword"}
    ]}],
    case network:setup(NetworkConfig) of
        ok ->
            timer:sleep(24*60*60*1000);
            %erlang:display(network:ifconfig());
        Error ->
            erlang:display(Error)
    end.

