-module(sta_network).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, "myssid"},
            {psk,  "mypsk"},
            {connected, fun() -> Self ! connected end},
            {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
            {disconnected, fun() -> Self ! disconnected end}
        ]}
    ],
    case network_fsm:start(Config) of
        ok ->
            wait_for_message();
        Error ->
            erlang:display(Error)
    end.


wait_for_message() ->
    receive
        connected ->
            erlang:display(connected);
        {ok, IpInfo} ->
            erlang:display(IpInfo);
        disconnected ->
            erlang:display(disconnected)
    after 15000 ->
        ok
    end,
    wait_for_message().
