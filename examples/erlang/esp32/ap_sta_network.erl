-module(ap_sta_network).

-export([start/0]).

start() ->
    Config = [
        {ap, [
            %% If an SSID is not specified, AtomVM will default to atomvm-<hexmac>
            %% where <hexmac> is the hexadecimal representation of the factory-supplied
            %% MAC address of the ESP32 device.
            %% {ssid, esp:nvs_get_binary(atomvm, ap_ssid, <<"myssid">>)},
            %% If a password is not specified, the AP network will be open, with
            %% no encryption or authentication (strongly discouraged)
            %% {psk,  esp:nvs_get_binary(atomvm, ap_psk, <<"mypsk">>)},
            {ap_started, fun ap_started/0},
            {sta_connected, fun sta_connected/1},
            {sta_ip_assigned, fun sta_ip_assigned/1},
            {sta_disconnected, fun sta_disconnected/1}
        ]},
        {sta, [
            {ssid, esp:nvs_get_binary(atomvm, sta_ssid, <<"myssid">>)},
            {psk,  esp:nvs_get_binary(atomvm, sta_psk, <<"mypsk">>)},
            {connected, fun connected/0},
            {got_ip, fun got_ip/1},
            {disconnected, fun disconnected/0}
        ]},
        {sntp, [{endpoint, "pool.ntp.org"}]}
    ],
    case network:start(Config) of
        ok ->
            sleep_forever();
        Error ->
            erlang:display(Error)
    end.


ap_started() ->
    io:format("AP started.~n").

sta_connected(Mac) ->
    io:format("STA connected with mac ~p~n", [Mac]).

sta_disconnected(Mac) ->
    io:format("STA disconnected with mac ~p~n", [Mac]).

sta_ip_assigned(Address) ->
    io:format("STA assigned address ~p~n", [Address]).



connected() ->
    io:format("STA connected.~n").

got_ip(IpInfo) ->
    io:format("Got IP: ~p.~n", [IpInfo]).

disconnected() ->
    io:format("STA disconnected.~n").


sleep_forever() ->
    timer:sleep(10000),
    sleep_forever().
