-module(copy_terms17).
-export([start/0, sort/1, insert/2, loop/0]).

start() ->
    Pid = spawn(?MODULE, loop, []),
    Ref = make_ref(),
    Ref2 = make_ref(),
    Pid ! {sort, Ref, self(), Ref2, [<<"Hello">>, <<"Ciao">>, <<"Hola">>, <<"Hi">>, <<"Hello World">>, <<"Bonjur">>]},
    Res =
        receive
            {reply, Ref, Sorted, Ref2} -> Sorted
        end,
    Pid ! terminate,
    [_, _, _, _, _, Last] = Res,
    byte_size(Last).

loop() ->
    case handle_request() of
        terminate ->
            terminate;

        ok ->
            loop()
    end.

handle_request() ->
    receive
        {sort, Ref, Pid, Ref2, L} ->
            Pid ! {reply, Ref, sort(L, []), Ref2},
            ok;

        terminate ->
            terminate
    end.

sort(L) ->
    sort(L, []).

sort([], Sorted) ->
    Sorted;

sort([H | Unsorted], Sorted) ->
    NextSorted = insert(Sorted, H),
    sort(Unsorted, NextSorted).

insert(L, I) ->
    insert(L, [], I).

insert([], HL, I) ->
    HL ++ [I];

insert([H | T], HL, I) when byte_size(I) < byte_size(H) ->
    HL ++ [I, H | T];

insert([H | T], HL, I) ->
    insert(T, HL ++ [H], I).
