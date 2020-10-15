-module(catch_from_other_module).

-export([start/0]).

start() ->
    A =
        try raise_badmatch:do_raise(-1, 3) of
            _Res -> -10
        catch
            error:{badmatch, 6} -> 1;
            _:_ -> 1024
        end,
    B =
        try raise_case_end:do_raise(1) of
            _Res2 -> -20
        catch
            error:{case_clause, 1} -> 2;
            _:_ -> 2048
        end,
    C =
        try raise_if_end:do_raise(0, 0) of
            _Res3 -> -40
        catch
            error:if_clause -> 4;
            _:_ -> 4096
        end,
    A + B + C.
