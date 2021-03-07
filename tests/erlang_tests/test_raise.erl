-module(test_raise).
-export([start/0]).

start() ->
    Pid = spawn(fun() -> loop(0) end),
    Tick = fun() -> Pid ! tick end,
    foo = tryit(
        fun() -> foo end,
        foo,
        fun() -> bar end,
        Tick
    ),
    bar = tryit(
        fun() -> throw(foo) end,
        foo,
        fun() -> bar end,
        Tick
    ),
    foo =
        try
            tryit(
                fun() -> throw(foo) end,
                bar,
                fun() -> bar end,
                Tick
            )
        catch
            _:E -> E
        end,
    caughtit = tryit(
        fun() ->
            tryit(
                fun() -> throw(passit) end,
                letitpass,
                fun() -> bar end,
                Tick
        )
        end,
        passit,
        fun() -> caughtit end,
        Tick
    ),
    caughtit = tryit(
        fun() ->
            tryit(
                fun() -> throw(passit) end,
                passit,
                fun() -> throw(throwfromcatch) end,
                Tick
        )
        end,
        throwfromcatch,
        fun() -> caughtit end,
        Tick
    ),
    Pid ! {self(), stop},
    receive X -> X end.

tryit(DoTry, CatchTerm, DoCatch, DoAfter) ->
    try
        DoTry()
    catch
        _:CatchTerm ->
            DoCatch()
    after
        DoAfter()
    end.

loop(Afters) ->
    receive
        tick ->
            loop(Afters + 1);
        {Pid, stop} ->
            Pid ! Afters
    end.
