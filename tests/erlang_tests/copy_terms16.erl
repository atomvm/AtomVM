-module(copy_terms16).

-export([start/0, find/2, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Ref = make_ref(),
    Pid ! {find, Ref, self(), [<<"Hello">>, <<"Ciao">>, <<"Hola">>, <<"Hi">>, <<"Bonjur">>]},
    Res =
        receive
            {reply, Ref, Longest} -> Longest
        end,
    Pid ! terminate,
    Res.

loop() ->
    case handle_request() of
        terminate ->
            terminate;
        ok ->
            loop()
    end.

handle_request() ->
    receive
        {find, Ref, Pid, List} ->
            Pid ! {reply, Ref, find(List, 0)},
            ok;
        terminate ->
            terminate
    end.

find([], Max) ->
    Max;
find([H | T], Max) when byte_size(H) > Max ->
    find(T, byte_size(H));
find([_H | T], Max) ->
    find(T, Max).
