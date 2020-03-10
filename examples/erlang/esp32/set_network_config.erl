-module(set_network_config).

-export([start/0]).

start() ->
    Ssid = <<"myssid">>,
    Psk = <<"mypsk">>,
    esp:nvs_set_binary(atomvm, sta_ssid, Ssid),
    erlang:display({atomvm, sta_ssid, Ssid}),
    esp:nvs_set_binary(atomvm, sta_psk,  Psk),
    erlang:display({atomvm, sta_psk, <<"xxxxxx">>}),
    ok.
