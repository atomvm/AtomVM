-module(tcp_client_esp32).

-export([start/0]).

-include("estdlib.hrl").
-include("atomvm.hrl").

start() ->
    try
        Self = self(),
        Config = [
            {sta, [
                {ssid, esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_SSID, <<"myssid">>)},
                {psk,  esp:nvs_get_binary(?ATOMVM_NVS_NS, ?ATOMVM_NVS_STA_PSK, <<"mypsk">>)},
                {connected, fun() -> Self ! connected end},
                {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
                {disconnected, fun() -> Self ! disconnected end}
            ]}
        ],
        case network_fsm:start(Config) of
            ok ->
                wait_for_network();
            Error ->
                erlang:display(Error)
        end
    catch
        _:E ->
            erlang:display(E)
    end.


wait_for_network() ->
    receive
        connected ->
            erlang:display(connected),
            wait_for_network();
        {ok, IpInfo} ->
            erlang:display(IpInfo),
            tcp_client_start();
        disconnected ->
            erlang:display(disconnected)
    end,
    infinite_loop().

infinite_loop() ->
    ?TIMER:sleep(1000),
    infinite_loop().

tcp_client_start() ->
    erlang:display("Connecting...\n"),
    Active = true,
    case ?GEN_TCP:connect("www.example.com", 80, [{active, Active}]) of
        {ok, Socket} ->
            erlang:display({connected, Socket}),
            case ?GEN_TCP:send(Socket, "GET / HTTP/1.0\r\n\r\n") of
                ok ->
                    case Active of
                        true -> active_receive_data();
                        _ -> passive_receive_data(Socket)
                    end;
                Error ->
                    erlang:display(Error)
            end;
        Error ->
            erlang:display(Error)
    end.

active_receive_data() ->
    receive
        {tcp_closed, _Socket} ->
            erlang:display(closed),
            ok;
        {tcp, _Socket, Packet} ->
            erlang:display(Packet),
            active_receive_data()
    end.

passive_receive_data(Socket) ->
    case ?GEN_TCP:recv(Socket, 128) of
        {error, closed} ->
            ok;
        {ok, Packet} ->
            erlang:display(Packet),
            passive_receive_data(Socket);
        Error ->
            erlang:display(Error)
    end.
