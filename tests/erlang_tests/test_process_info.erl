-module(test_process_info).
-export([start/0, loop/2]).

start() ->
    Self = self(),
    Pid = spawn(?MODULE, loop, [Self, []]), receive ok -> ok end,
    test_message_queue_len(Pid, Self),
    Pid ! {Self, stop}, receive X -> 0 end.

test_message_queue_len(Pid, Self) ->
    {message_queue_len, MessageQueueLen} = process_info(Pid, message_queue_len),
    {memory, Memory} = process_info(Pid, memory),
    {heap_size, HeapSize} = process_info(Pid, heap_size),
    Pid ! incr,
    Pid ! incr,
    Pid ! incr,
    {message_queue_len, MessageQueueLen2} = process_info(Pid, message_queue_len),
    {memory, Memory2} = process_info(Pid, memory),
    Pid ! {Self, ping}, receive pong -> ok end,
    {heap_size, HeapSize2} = process_info(Pid, heap_size),
    assert(MessageQueueLen < MessageQueueLen2),
    assert(Memory < Memory2),
    assert(HeapSize < HeapSize2).

loop(undefined, Accum) ->
    receive
        {Pid, stop} ->
            Pid ! Accum;
        incr ->
            loop(undefined, [incr | Accum]);
        {Pid, ping} ->
            Pid ! pong,
            loop(undefined, Accum)
    end;
loop(Pid, Accum) ->
    Pid ! ok,
    loop(undefined, Accum).

assert(true) -> ok.
