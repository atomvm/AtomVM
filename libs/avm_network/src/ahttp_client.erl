%
% This file is part of AtomVM.
%
% Copyright 2024 Davide Bettio <davide@uninstall.it>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(ahttp_client).
-export([connect/4, request/5, stream/2, stream_request_body/3, recv/2, close/1]).

-export_type([connection/0, error_tuple/0, backend/0]).

-define(DEFAULT_WANTED_HEADERS, [<<"Content-Length">>]).

-type maybe_binary() :: binary() | undefined.
-type maybe_integer() :: integer() | undefined.
-type maybe_ref() :: reference() | undefined.
-type maybe_parsing_state() :: headers | body | done | undefined.

-record(parser_state, {
    state :: maybe_parsing_state(),
    acc :: maybe_binary(),
    remaining_body_bytes :: maybe_integer(),
    last_header :: maybe_binary() | ignore,
    wanted_headers :: [binary()]
}).

-type maybe_parser_state() :: #parser_state{} | undefined.

-type gen_tcp_socket() :: {gen_tcp, inet:socket()}.
-type ssl_socket() :: {ssl, ssl:sslsocket()}.
-type tagged_socket() :: gen_tcp_socket() | ssl_socket().

-record(http_client, {
    host :: binary(),
    socket :: tagged_socket(),
    parser :: maybe_parser_state(),
    ref :: maybe_ref(),
    parse_headers :: [binary()]
}).

-opaque connection() :: #http_client{}.

-type protocol() :: http | https.
-type host() :: binary() | string().
-type option() :: gen_tcp:option() | ssl:tls_client_option() | {parse_headers, [binary()]}.

-type backend() :: gen_tcp | ssl.

-type socket_message() :: {tcp, inet:socket(), binary()} | {ssl, ssl:sslsocket(), binary()}.

-type status_response() :: {status, reference(), 0..999}.
-type header_response() :: {header, reference(), {binary(), binary()}}.
-type header_continuation_response() :: {header_continuation, reference(), {binary(), binary()}}.
-type data_response() :: {data, reference(), binary()}.
-type done_response() :: {done, reference()}.

-type response() ::
    status_response()
    | header_response()
    | header_continuation_response()
    | data_response()
    | done_response().

-type error_tuple() :: {error, {backend(), term()}}.

%%-----------------------------------------------------------------------------
%% @param   Protocol the protocol, either http or https
%% @param   Host the server hostname
%% @param   Port the server port number, usually 80 (http) or 443 (https)
%% @param   Options the property list with http client and connection options
%% @returns Either a http connection result tuple or an error tuple
%% @doc     Connects to the http(s) server.
%%
%%          The `Host' parameter may be a fully qualified host name or a string
%%          containing a valid dotted pair IP address.  (Currently, only IPv4 is
%%          supported). `Host' can be also a binary.
%%
%%          The `Options' can be used for providing connection options such as SSL
%%          `{verify, verify_none}', `gen_tcp' options such as `{active, false}' and `ahttp_client'
%%          `{parse_headers, [<<"HeaderName">>]}'.
%% @end
%%-----------------------------------------------------------------------------
-spec connect(
    Protocol :: protocol(), Host :: host(), Port :: inet:port_number(), Options :: [option()]
) -> {ok, connection()} | error_tuple().

connect(Protocol, Host, Port, Options) when is_binary(Host) ->
    connect(Protocol, binary_to_list(Host), Port, Options);
connect(http, Host, Port, Options) ->
    {ParseHeaders, ConnOptions} = take_parse_headers(Options),
    case gen_tcp:connect(Host, Port, transform_options(ConnOptions)) of
        {ok, Sock} -> init(Host, {gen_tcp, Sock}, ParseHeaders);
        {error, Error} -> {error, {gen_tcp, Error}}
    end;
connect(https, Host, Port, Options) ->
    {ParseHeaders, ConnOptions} = take_parse_headers(Options),
    case ssl:connect(Host, Port, transform_options(ConnOptions)) of
        {ok, Sock} -> init(Host, {ssl, Sock}, ParseHeaders);
        {error, Error} -> {error, {ssl, Error}}
    end.

init(Host, TSocket, ParseHeaders) ->
    {ok, #http_client{
        host = iolist_to_binary(Host), socket = TSocket, parse_headers = ParseHeaders
    }}.

transform_options(Options) ->
    WithActive =
        case proplists:get_value(active, Options) of
            undefined -> [{active, true} | Options];
            _OtherValue -> Options
        end,
    [binary | WithActive].

take_parse_headers(Options) ->
    case proplists:get_value(parse_headers, Options) of
        undefined -> {[], Options};
        Value -> {Value, proplists:delete(parse_headers, Options)}
    end.

%%-----------------------------------------------------------------------------
%% @param   Conn the connection
%% @param   Method a http method such as "GET", "POST", "PUT", etc...
%% @param   Path the path to the http resource, such as "/"
%% @param   Headers a list of headers
%% @param   Body the body that is sent to the server, may be `undefined' or `nil' when there is no
%%          body
%% @returns Either a result tuple with the updated http connection and a reference to the http
%%          request, or an error tuple.
%% @doc     Makes a http request using given method on provided path.
%%
%%          When using methods such as `GET' the body should be omited using either `undefined' or
%%          `nil' (they are both equivalent).
%%
%%          When uploading a smaller body (a single binary that fits in memory) the body binary can
%%          be provided.
%%
%%          `stream' option can be used with `stream_request_body/3' in order to upload a bigger
%%          binary in streaming mode. This option should be combined with `Content-Length' header.
%%
%%          As soon as the request is sent to the server, a tuple such as `{ok, Conn, Ref}' is
%%          returned, otherwise an error tuple is returned, such as
%%          `{error, {gen_tcp, econnrefused}}'.
%%
%%          The returned connection should be used for the next call, such as to `stream/2'
%%          (when using active mode) or `recv/3' (when using passive mode).
%%          `Ref' is meant to identify a single request, so any response to a specific request will
%%          be identified from the same reference.
%% @end
%%-----------------------------------------------------------------------------
-spec request(
    Conn :: connection(),
    Method :: iodata(),
    Path :: iodata(),
    Headers :: [iodata()],
    Body :: binary() | undefined | nil | stream
) -> {ok, connection(), reference()} | error_tuple().
request(
    #http_client{host = Host, socket = Socket} = Conn, Method, Path, Headers, Body
) ->
    {BodyBin, MaybeBodyHeaders} = prepare_body(Body, Headers),
    HeadersList = transform_headers(MaybeBodyHeaders),
    Data = iolist_to_binary([
        Method,
        $\s,
        Path,
        <<" HTTP/1.1\r\n">>,
        <<"Host: ">>,
        Host,
        <<"\r\n">>,
        HeadersList,
        <<"\r\n">>,
        BodyBin
    ]),
    case send(Socket, Data) of
        ok ->
            Ref = make_ref(),
            WantedHeaders = ?DEFAULT_WANTED_HEADERS ++ Conn#http_client.parse_headers,
            {ok,
                Conn#http_client{ref = Ref, parser = #parser_state{wanted_headers = WantedHeaders}},
                Ref};
        {error, _} = Error ->
            Error
    end.

send({gen_tcp, TCPSocket}, Data) ->
    case gen_tcp:send(TCPSocket, Data) of
        ok -> ok;
        {error, Error} -> {error, {gen_tcp, Error}}
    end;
send({ssl, SSLSocket}, Data) ->
    case ssl:send(SSLSocket, Data) of
        ok -> ok;
        {error, Error} -> {error, {ssl, Error}}
    end.

prepare_body(undefined, Headers) ->
    {<<"">>, Headers};
prepare_body(nil, Headers) ->
    {<<"">>, Headers};
prepare_body(stream, Headers) ->
    {<<"">>, Headers};
prepare_body(Body, Headers) ->
    BodyLen = integer_to_binary(byte_size(Body)),
    {Body, [{<<"Content-Length">>, BodyLen} | Headers]}.

transform_headers([]) ->
    [];
transform_headers([{Name, Value} | Tail]) ->
    [Name, <<": ">>, Value, <<"\r\n">> | transform_headers(Tail)].

%%-----------------------------------------------------------------------------
%% @param   Conn the connection
%% @param   Msg a received message
%% @returns Either a list of responses, `unknown' or an error tuple.
%% @doc     This function should be used when in active mode in order to process socket messages.
%%
%%          If a socket message is streamed using this function, a tuple with a list of http
%%          responses is returned (e.g. `{ok, UpdatedConn, Responses}`', or an error tuple.
%%
%%          Otherwise `unknown' is returned, that means that the message is not a socket message
%%          tied to the open connection, and it should be handled in some other way.
%%
%%          The first returned response to a new request is a status `{status, Ref, 200}' for
%%          a successful response. After that headers and data may follow.
%%
%%          Since the response might span multiple socket messages, `stream/2' may be called
%%          multiple times. Each time the latest `UpdatedConn' must be used.
%% @end
%%-----------------------------------------------------------------------------
-spec stream(Conn :: connection(), Msg :: socket_message()) ->
    {ok, connection(), [response()]} | {ok, connection(), closed} | unknown | error_tuple().

stream(#http_client{socket = {gen_tcp, TSocket}, parser = Parser} = Conn, {tcp, TSocket, Chunk}) ->
    {ok, UpdatedParser, Parsed} = feed_parser(Parser, Chunk),
    Responses = make_responses(Parsed, Conn#http_client.ref, []),
    {ok, Conn#http_client{parser = UpdatedParser}, Responses};
stream(#http_client{socket = {gen_tcp, TSocket}} = Conn, {tcp_closed, TSocket}) ->
    {ok, Conn, closed};
stream(#http_client{socket = {ssl, SSLSocket}, parser = Parser} = Conn, {ssl, SSLSocket, Chunk}) ->
    {ok, UpdatedParser, Parsed} = feed_parser(Parser, Chunk),
    Responses = make_responses(Parsed, Conn#http_client.ref, []),
    {ok, Conn#http_client{parser = UpdatedParser}, Responses};
stream(_Conn, _Other) ->
    unknown.

stream_data(#http_client{parser = Parser} = Conn, Chunk) ->
    {ok, UpdatedParser, Parsed} = feed_parser(Parser, Chunk),
    Responses = make_responses(Parsed, Conn#http_client.ref, []),
    {ok, Conn#http_client{parser = UpdatedParser}, Responses}.

make_responses([], _Ref, Acc) ->
    Acc;
make_responses([Response | Tail], Ref, Acc) when is_atom(Response) ->
    make_responses(Tail, Ref, [{Response, Ref} | Acc]);
make_responses([{Tag, Value} | Tail], Ref, Acc) ->
    make_responses(Tail, Ref, [{Tag, Ref, Value} | Acc]).

feed_parser(#parser_state{state = body} = Parser, Chunk) ->
    consume_bytes(append_chunk(Parser, Chunk), []);
feed_parser(Parser, Chunk) ->
    consume_lines(append_chunk(Parser, Chunk), []).

consume_bytes(#parser_state{acc = undefined} = Parser, ParsedAcc) ->
    {ok, Parser, ParsedAcc};
consume_bytes(#parser_state{acc = Chunk} = Parser, ParsedAcc) when is_binary(Chunk) ->
    ReplacedAccParser = replace_chunk(Parser, undefined),
    NewRemBodyBytes = maybe_decrement(
        ReplacedAccParser#parser_state.remaining_body_bytes, byte_size(Chunk)
    ),
    {UpdatedParser, Parsed} = maybe_handle_end_of_response(
        ReplacedAccParser#parser_state{remaining_body_bytes = NewRemBodyBytes}, [
            {data, Chunk} | ParsedAcc
        ]
    ),
    {ok, UpdatedParser, Parsed}.

consume_lines(#parser_state{acc = undefined} = Parser, ParsedAcc) ->
    {ok, Parser, ParsedAcc};
consume_lines(Parser, ParsedAcc) ->
    case binary:split(Parser#parser_state.acc, <<"\r\n">>) of
        [_NotTerminatedLine] ->
            {ok, Parser, ParsedAcc};
        [Line, Rest] ->
            ReplacedAccParser = replace_chunk(Parser, Rest),
            case parse_line(ReplacedAccParser, Line) of
                {consume_bytes, UpdatedParser} ->
                    consume_bytes(UpdatedParser, ParsedAcc);
                {ok, UpdatedParser} ->
                    consume_lines(UpdatedParser, ParsedAcc);
                {ok, UpdatedParser, Found} ->
                    consume_lines(UpdatedParser, [Found | ParsedAcc]);
                {error, UpdatedParser, NotParsed} ->
                    {error, UpdatedParser, ParsedAcc, NotParsed}
            end
    end.

append_chunk(#parser_state{acc = undefined} = Parser, Chunk) ->
    Parser#parser_state{acc = Chunk};
append_chunk(#parser_state{acc = Acc} = Parser, Chunk) ->
    Parser#parser_state{acc = <<Acc/binary, Chunk/binary>>}.

replace_chunk(Parser, <<>>) ->
    Parser#parser_state{acc = undefined};
replace_chunk(Parser, Chunk) ->
    Parser#parser_state{acc = Chunk}.

parse_line(
    #parser_state{state = undefined} = Parser, <<"HTTP/1.0 ", C:3/binary, " ", _Txt/binary>>
) ->
    StatusCode = binary_to_integer(C),
    {ok, Parser#parser_state{state = headers}, {status, StatusCode}};
parse_line(
    #parser_state{state = undefined} = Parser, <<"HTTP/1.1 ", C:3/binary, " ", _Txt/binary>>
) ->
    StatusCode = binary_to_integer(C),
    {ok, Parser#parser_state{state = headers}, {status, StatusCode}};
parse_line(#parser_state{state = headers} = Parser, <<>>) ->
    {consume_bytes, Parser#parser_state{state = body}};
parse_line(
    #parser_state{state = headers, last_header = ignore} = Parser, <<C, _MultiLine/binary>>
) when C == $\s orelse C == $\t ->
    {ok, Parser};
parse_line(
    #parser_state{state = headers, last_header = LastH} = Parser, <<C, MultiLine/binary>>
) when is_binary(LastH) andalso (C == $\s orelse C == $\t) ->
    LTrimmedValue = trim_left_spaces(MultiLine, 0),
    TrimmedValue = trim_right_spaces(LTrimmedValue, byte_size(LTrimmedValue)),
    {ok, Parser, {header_continuation, {LastH, TrimmedValue}}};
parse_line(#parser_state{state = headers, wanted_headers = WantedHeaders} = Parser, HeaderLine) ->
    case match_header(WantedHeaders, HeaderLine) of
        {ok, Name, Value} ->
            LTrimmedValue = trim_left_spaces(Value, 0),
            TrimmedValue = trim_right_spaces(LTrimmedValue, byte_size(LTrimmedValue)),
            UpdatedParser =
                case Name of
                    % this is safe since match_header uses same casing as in WantedHeaders
                    <<"Content-Length">> ->
                        RemainingLen = binary_to_integer(TrimmedValue),
                        Parser#parser_state{remaining_body_bytes = RemainingLen};
                    _ ->
                        Parser
                end,
            {ok, UpdatedParser#parser_state{last_header = Name}, {header, {Name, TrimmedValue}}};
        ignore ->
            {ok, Parser#parser_state{last_header = ignore}};
        error ->
            {error, Parser, HeaderLine}
    end;
parse_line(Parser, Any) ->
    {error, Parser, Any}.

trim_left_spaces(Bin, Count) ->
    case Bin of
        <<_Bin:Count/binary, C, _Rest/binary>> when C == $\s orelse C == $\t ->
            trim_left_spaces(Bin, Count + 1);
        <<_Bin:Count/binary, NoLeftSpaces/binary>> ->
            NoLeftSpaces
    end.

trim_right_spaces(Bin, Count) ->
    Len = Count - 1,
    case Bin of
        <<_LBin:Len/binary, C, _TrimmedSpaces/binary>> when C == $\s orelse C == $\t ->
            trim_right_spaces(Bin, Count - 1);
        <<LBin:Count/binary, _TrimmedSpaces/binary>> ->
            LBin
    end.

maybe_decrement(undefined, _B) ->
    undefined;
maybe_decrement(A, B) ->
    A - B.

maybe_handle_end_of_response(#parser_state{remaining_body_bytes = 0} = ParserState, AlreadyParsed) ->
    {ParserState#parser_state{state = done}, [done | AlreadyParsed]};
maybe_handle_end_of_response(ParserState, AlreadyParsed) ->
    {ParserState, AlreadyParsed}.

match_header([], _HeaderLine) ->
    ignore;
match_header([Name | Tail], HeaderLine) ->
    NameLen = byte_size(Name),
    case HeaderLine of
        <<Name:NameLen/binary, $:, Value/binary>> ->
            {ok, Name, Value};
        <<MaybeName:NameLen/binary, $:, Value/binary>> ->
            case icmp(Name, MaybeName) of
                true -> {ok, Name, Value};
                false -> match_header(Tail, HeaderLine)
            end;
        _NotMatched ->
            match_header(Tail, HeaderLine)
    end.

icmp(Bin1, Bin2) ->
    icmp(Bin1, Bin2, byte_size(Bin1)).

icmp(_Bin1, _Bin2, 0) ->
    true;
icmp(Bin1, Bin2, Len) ->
    PrefixLen = Len - 1,
    <<_L1:PrefixLen/binary, C1, _R1/binary>> = Bin1,
    <<_L2:PrefixLen/binary, C2, _R2/binary>> = Bin2,
    case tolower(C1) == tolower(C2) of
        true -> icmp(Bin1, Bin2, Len - 1);
        false -> false
    end.

tolower(C) when C >= $A andalso C =< $Z ->
    C + ($a - $A);
tolower(C) ->
    C.

%%-----------------------------------------------------------------------------
%% @param   Conn the connection
%% @param   Ref the reference to the pending request
%% @param   BodyChunk a chunk of the body that will be sent
%% @returns Either `ok' or an error tuple.
%% @doc     Uploads a chunk of request body.
%%
%%          This function should be used when `stream' has been used as Body parameter.
%% @end
%%-----------------------------------------------------------------------------
-spec stream_request_body(Conn :: connection(), Ref :: reference(), BodyChunk :: binary()) ->
    ok | error_tuple().

stream_request_body(#http_client{socket = Socket, ref = Ref} = Conn, Ref, BodyChunk) ->
    Data = iolist_to_binary(BodyChunk),
    case send(Socket, Data) of
        ok -> {ok, Conn, Ref};
        {error, _} = Error -> Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Conn the connection
%% @param   Len the number of bytes will be received, when using 0 all pending bytes are received
%% @returns Either an ok tuple with the updated connection and a list of responses or an error
%%          tuple.
%% @doc     Receive and parse a number of bytes from the http connection.
%%
%%          This function should be used when the connection has been opened using
%%          `{active, false}'.
%%
%%          See also `stream/2' for more information about the responses list.
%% @end
%%-----------------------------------------------------------------------------
-spec recv(Conn :: connection(), Len :: non_neg_integer()) ->
    {ok, connection(), [response()]} | error_tuple().

recv(#http_client{socket = {SocketType, _}} = Conn, Len) ->
    case socket_recv(Conn, Len) of
        {ok, Data} -> stream_data(Conn, Data);
        {error, Reason} -> {error, {SocketType, Reason}}
    end.

socket_recv(#http_client{socket = {gen_tcp, TCPSocket}}, Len) ->
    gen_tcp:recv(TCPSocket, Len);
socket_recv(#http_client{socket = {ssl, SSLSocket}}, Len) ->
    ssl:recv(SSLSocket, Len).

%%-----------------------------------------------------------------------------
%% @param   Conn the connection
%% @returns Either `ok' or an error tuple.
%% @doc     Closes the connection.
%% @end
%%-----------------------------------------------------------------------------
-spec close(Conn :: connection()) -> ok | error_tuple().

close(#http_client{socket = {gen_tcp, TCPSocket}}) ->
    case gen_tcp:close(TCPSocket) of
        ok -> ok;
        Error -> {error, {gen_tcp, Error}}
    end;
close(#http_client{socket = {ssl, SSLSocket}}) ->
    case ssl:close(SSLSocket) of
        ok -> ok;
        Error -> {error, {ssl, Error}}
    end.
