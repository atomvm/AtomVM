-module(test_gen_udp).

-export([test/0, start_sender/3]).

-include("estdlib.hrl").
-include("etest.hrl").

test() ->
    ok = test_send_receive_active(),
    ok.

test_send_receive_active() ->
    {ok, Socket} = ?GEN_UDP:open(0, [{active, true}]),
    {ok, Port} = ?INET:port(Socket),
    NumToSend = 10,
    Sender = erlang:spawn(?MODULE, start_sender, [Socket, Port, make_messages(NumToSend)]),
    NumReceived = count_received(),
    Sender ! stop,
    ?ASSERT_TRUE((0 < NumReceived) and (NumReceived =< NumToSend)),
    ok = ?GEN_UDP:close(Socket),
    ok.


make_messages(0) ->
    [];
make_messages(N) ->
    [<<"foo">> | make_messages(N - 1)].

start_sender(Socket, Port, Msgs) ->
    send(Socket, Port, Msgs),
    receive stop -> 
        ok 
    end.

send(_Socket, _Port, []) ->
    ok;
send(Socket, Port, [Msg | Rest]) ->
    ?GEN_UDP:send(Socket, {127,0,0,1}, Port, Msg),
    send(Socket, Port, Rest).

count_received() ->
    count_received(0).

count_received(I) ->
    receive
        _Msg ->
            count_received(I + 1)
    after 100 ->
        I
    end.
