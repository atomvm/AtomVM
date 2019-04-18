-module(udp_client).

-export([start/0]).

-include("estdlib.hrl").

start() ->
    console:puts("Opening socket...\n"),
    Socket = ?GEN_UDP:open(0),
    loop(Socket).

loop(Socket) ->
    console:puts("."),
    console:flush(),
    case ?GEN_UDP:send(Socket, {127,0,0,1}, 44444, ":アトムＶＭ") of
        ok ->
            ok;
        {error, Reason} ->
            console:puts("An error ocurred: "), erlang:display(Reason)
    end,
    ?TIMER:sleep(5000),
    loop(Socket).
