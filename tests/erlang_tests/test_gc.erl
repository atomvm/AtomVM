-module(test_gc).
-export([start/0]).

start() ->
    {HeapSize, _} = make_a_big_heap(),
    MemorySize = erlang:process_info(self(), memory),
    true = erlang:garbage_collect(),
    NewHeapSize = erlang:process_info(self(), heap_size),
    ok = case NewHeapSize < HeapSize of
        true -> ok;
        _ -> fail
    end,
    NewMemorySize = erlang:process_info(self(), memory),
    ok = case NewMemorySize < MemorySize of
        true -> ok;
        _ -> fail
    end,
    0.

make_a_big_heap() ->
    LargeBlob = create_string(1024, []),
    HeapSize = erlang:process_info(self(), heap_size),
    {HeapSize, length(LargeBlob)}.


create_string(0, Accum) ->
    Accum;
create_string(Len, Accum) ->
    create_string(Len - 1, [Len rem 256 | Accum]).
