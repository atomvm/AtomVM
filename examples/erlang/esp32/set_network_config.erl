-module(set_network_config).

-export([start/0]).

-include("atomvm.hrl").

start() ->
    Ssid = <<"myssid">>,
    Psk = <<"mypsk">>,
    esp:nvs_set_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_SSID, Ssid),
    erlang:display({?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_SSID, Ssid}),
    esp:nvs_set_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_PSK,  Psk),
    erlang:display({?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_PSK, <<"xxxxxx">>}),
    ok.
