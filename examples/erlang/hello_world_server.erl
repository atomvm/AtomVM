-module(hello_world_server).

-export([start/0, handle_req/3]).

-include("estdlib.hrl").

start() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, "SSID"},
            {psk,  "PSK"},
            {connected, fun() -> Self ! connected end},
            {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
            {disconnected, fun() -> Self ! disconnected end}
        ]}
    ],
    case network_fsm:start(Config) of
        ok ->
            wait_for_message();
        Error ->
            ?IO:format("An error occurred starting network: ~p~n", [Error])
    end.

handle_req("GET", [], Conn) ->
    Body = <<"<html><body><h1>Hello World</h1></body></html>">>,
    http_server:reply(200, Body, Conn);

handle_req(Method, Path, Conn) ->
    ?IO:format("Method: ~p Path: ~p~n", [Method, Path]),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).

wait_for_message() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    receive
        connected ->
            ?IO:format("Connected~n");
        {ok, IpInfo} ->
            ?IO:format("Acquired IP address: ~p~n", [IpInfo]),
            http_server:start_server(8080, Router);
        disconnected ->
            ?IO:format("Disonnected~n")
    after 15000 ->
        ok
    end,
    wait_for_message().
