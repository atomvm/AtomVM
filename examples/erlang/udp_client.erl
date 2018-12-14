-module(udp_client).

-export([start/0]).

start() ->
    Console = console:start(),
    console:puts(Console, "Opening socket...\n"),
    Socket = gen_udp:open(0),
    loop(Console, Socket).

loop(Console, Socket) ->
    console:puts(Console, "Sending foo...\n"),
    case gen_udp:send(Socket, {127,0,0,1}, 44444, "foo") of
        ok ->
            ok;
        {error, Reason} ->
            console:puts(Console, "An error ocurred: "), erlang:display(Reason)
    end,
    timer:sleep(500),
    loop(Console, Socket).
