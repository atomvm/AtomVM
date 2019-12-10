-module(system_info_server).

-export([start/0, handle_req/3]).

-include("estdlib.hrl").

start() ->
    Self = self(),
    Config = [
        {sta, [
            {ssid, "SSID"},
            {psk,  "PSK"},
            {connected, fun() -> Self ! connected end},
            {got_ip, fun(IpInfo) -> Self ! {ok, IpInfo} end},
            {disconnected, fun() -> Self ! disconnected end}
        ]}
    ],
    case network_fsm:start(Config) of
        ok ->
            wait_for_message();
        Error ->
            ?IO:format("An error occurred starting network: ~p~n", [Error])
    end.

wait_for_message() ->
    Router = [
        {"*", ?MODULE, []}
    ],
    receive
        connected ->
            ?IO:format("Connected~n");
        {ok, IpInfo} ->
            ?IO:format("Acquired IP address: ~p~n", [IpInfo]),
            http_server:start_server(8080, Router);
        disconnected ->
            ?IO:format("Disonnected~n")
    after 15000 ->
        ok
    end,
    wait_for_message().

handle_req("GET", [], Conn) ->
    TimeString = universaltime_to_bin(erlang:universaltime()),
    Body = [<<"<html><body><h1>">>, TimeString, <<"</h1></body></html>">>],
    http_server:reply(200, Body, Conn);

handle_req("GET", ["system", "info"], Conn) ->
    SysInfo = [
        {atom_count, erlang:system_info(atom_count)},
        {process_count, erlang:system_info(process_count)},
        {port_count, erlang:system_info(port_count)},
        {word_size, erlang:system_info(wordsize)},
        {system_architecture, erlang:system_info(system_architecture)}
    ],
    Body = json_encoder:encode(SysInfo),
    http_server:reply(200, Body, Conn);

handle_req("GET", ["processes", PidString, "info"], Conn) ->
    {Code, ProcInfo} = try_proc_info_list(PidString),
    Body = json_encoder:encode(ProcInfo),
    http_server:reply(Code, Body, Conn);

handle_req(Method, Path, Conn) ->
    ?IO:format("Method: ~p Path: ~p~n", [Method, Path]),
    Body = <<"<html><body><h1>Not Found</h1></body></html>">>,
    http_server:reply(404, Body, Conn).

universaltime_to_bin({{Year, Month, Day}, {H, M, S}}) ->
    [
     erlang:integer_to_binary(Year), $/,
     erlang:integer_to_binary(Month), $/,
     erlang:integer_to_binary(Day), $\s,
     erlang:integer_to_binary(H), $:,
     erlang:integer_to_binary(M), $:,
     erlang:integer_to_binary(S)
    ].

try_proc_info_list(PidString) ->
    try proc_info_list(PidString) of
        Res -> {200, Res}
    catch
        _:_ -> {404, [{error, <<"Process not found.">>}]}
    end.

proc_info_list(PidString) ->
    PidInteger = erlang:list_to_integer(PidString),
    Procs = erlang:processes(),
    Pid = ?LISTS:nth(PidInteger, Procs),
    ?IO:format("pid: ~p~n", [Pid]),
    [
        erlang:process_info(Pid, heap_size),
        erlang:process_info(Pid, stack_size),
        erlang:process_info(Pid, message_queue_len),
        erlang:process_info(Pid, memory)
    ].
