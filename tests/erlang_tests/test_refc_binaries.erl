-module(test_refc_binaries).

-export([start/0]).

-define(LITERAL_BIN,
    <<"0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789">>
).

-record(state, {
    bin
}).

start() ->
    ok = run_test(fun() -> test_heap_binary() end),
    ok = run_test(fun() -> test_const_binary() end),
    ok = run_test(fun() -> test_non_const_binary() end),
    ok = run_test(fun() -> test_shared() end),
    0.

test_heap_binary() ->
    HeapSize0 = get_heap_size(),
    Bin = create_binary(63),
    HeapSize1 = get_heap_size(),
    true = HeapSize0 < HeapSize1,
    true = erlang:byte_size(Bin) < HeapSize1,
    ok.

test_const_binary() ->
    HeapSize0 = get_heap_size(),
    BinarySize = erlang:byte_size(?LITERAL_BIN),
    true = HeapSize0 < BinarySize,
    ok.

test_non_const_binary() ->
    HeapSize0 = get_heap_size(),
    String = create_string(1024),
    HeapSize1 = get_heap_size(),
    true = HeapSize0 < HeapSize1,
    Bin = create_binary(String),
    HeapSize2 = get_heap_size(),
    true = HeapSize1 < HeapSize2,
    true = HeapSize2 < (HeapSize1 + erlang:byte_size(Bin)),
    id(String), id(Bin),
    ok.

test_shared() ->
    Bin = create_binary(1024),
    Pid = erlang:spawn(fun() -> loop(#state{}) end),
    PidHeapSize0 = get_heap_size(Pid),
    %%
    %% Send the process a refc binary, and check heap size
    %%
    ok = send(Pid, {ref, Bin}),
    PidHeapSize1 = get_heap_size(Pid),
    true = PidHeapSize0 < PidHeapSize1,
    true = PidHeapSize1 < 1024,
    %%
    %% Make sure we can get what we sent
    %%
    Bin = send(Pid, get),
    %%
    %% Free the refc binary; heap should decrease
    %%
    ok = send(Pid, free),
    PidHeapSize2 = get_heap_size(Pid),
    true = PidHeapSize2 < PidHeapSize1,
    ok.

get_heap_size() ->
    get_heap_size(self()).

get_heap_size(Pid) ->
    {heap_size, Size} = erlang:process_info(Pid, heap_size),
    Size * erlang:system_info(wordsize).

send(Pid, Msg) ->
    Ref = erlang:make_ref(),
    Pid ! {self(), Ref, Msg},
    receive
        {Ref, Reply} -> Reply
    end.

loop(State) ->
    erlang:garbage_collect(),
    receive
        {Pid, Ref, get} ->
            Pid ! {Ref, State#state.bin},
            loop(State);
        {Pid, Ref, free} ->
            Pid ! {Ref, ok},
            loop(State#state{bin=undefined});
        {Pid, Ref, {ref, Bin}} ->
            Pid ! {Ref, ok},
            loop(State#state{bin=Bin});
        {Pid, Ref, halt} ->
            Pid ! {Ref, ok}
    end.

create_binary(N) when is_integer(N) ->
    erlang:list_to_binary(create_string(N, []));
create_binary(S) when is_list(S) ->
    list_to_binary(S).

create_string(N) ->
    create_string(N, []).

create_string(0, Accum) ->
    Accum;
create_string(N, Accum) ->
    create_string(N - 1, [N rem 256 | Accum]).


run_test(Fun) ->
    Self = self(),
    _Pid = spawn(fun() -> execute(Self, Fun) end),
    receive
        ok ->
            ok;
        Error ->
            Error
    end.

execute(Pid, Fun) ->
    Result = try
        Fun(), ok
    catch
        _:Error ->
            {error, Error}
    end,
    Pid ! Result.

id(X) -> X.
