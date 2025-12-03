%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

%%-----------------------------------------------------------------------------
%% @doc Code server
%%
%% This module is responsible for performing JIT compilation of module
%% @end
%%-----------------------------------------------------------------------------
-module(code_server).

% API
-export([
    start_link/0
]).

% NIFs
-export([
    is_loaded/1,
    resume/2,
    code_chunk/1,
    atom_resolver/2,
    literal_resolver/2,
    type_resolver/2,
    set_native_code/3
]).

% gen_server
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% @doc Start code server
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @hidden
init([]) ->
    State = [],
    {ok, State}.

%% @hidden
handle_call(_Message, _From, State) ->
    {noreply, State}.

%% @hidden
handle_cast(_Message, State) ->
    {noreply, State}.

%% @hidden
handle_info({load, Module, TrappedPid}, State) ->
    LoadResult =
        case code_server:is_loaded(Module) of
            true -> ok;
            false -> load(Module)
        end,
    code_server:resume(TrappedPid, LoadResult),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Determine if a given module is loaded
%% @return true if the module is loaded
%% @param Module module to test
-spec is_loaded(atom()) -> boolean().
is_loaded(_Module) ->
    erlang:nif_error(undefined).

%% @doc Resume a process that was trapped waiting for module to be loaded
%% @return ok
%% @param Pid process id to resume
%% @param LoadResult result of the load operation
-spec resume(Pid :: pid(), LoadResult :: ok | undef) -> true.
resume(_Pid, _LoadResult) ->
    erlang:nif_error(undefined).

%% @doc Get the bytecode of a given module
%% @return Bytecode of the module, as a binary
%% @param Module module to get the bytecode of
-spec code_chunk(Module :: module()) -> binary().
code_chunk(_Module) ->
    erlang:nif_error(undefined).

%% @doc Get the atom from the atom module index
%% @return The atom
%% @param Module module get the atom of
%% @param Index atom index in the module
-spec atom_resolver(Module :: module(), Index :: non_neg_integer()) -> atom().
atom_resolver(_Module, _Index) ->
    erlang:nif_error(undefined).

%% @doc Get a module literal from its index
%% @return The module literal
%% @param Module module get a literal from
%% @param Index literal index in the module
-spec literal_resolver(Module :: module(), Index :: non_neg_integer()) -> any().
literal_resolver(_Module, _Index) ->
    erlang:nif_error(undefined).

%% @doc Get a type from its index
%% @return The type information
%% @param Module module get a type from
%% @param Index type index in the module
-spec type_resolver(Module :: module(), Index :: non_neg_integer()) -> any().
type_resolver(_Module, _Index) ->
    erlang:nif_error(undefined).

%% @doc Associate a native code stream with a module
%% @return ok
%% @param Module module to set the native code of
%% @param LabelsCount number of labels in the module
%% @param Stream resource describing the stream containing native code
-spec set_native_code(Module :: module(), LabelsCount :: pos_integer(), Stream :: jit:stream()) ->
    ok.
set_native_code(_Module, _LabelsCount, _Stream) ->
    erlang:nif_error(undefined).

%% @doc Load a given module, performing jit compilation.
%% @return ok
%% @param Module module to load
-spec load(module()) -> ok | undef.
load(Module) ->
    case erlang:system_info(emu_flavor) of
        jit ->
            % atomvm_heap_growth, fibonacci reduces compilation time
            {Pid, Ref} = spawn_opt(
                fun() ->
                    try
                        io:format("Compilation of ~s...", [Module]),
                        Start = erlang:system_time(millisecond),
                        Code = code_server:code_chunk(Module),
                        AtomResolver = fun(Index) -> code_server:atom_resolver(Module, Index) end,
                        LiteralResolver = fun(Index) ->
                            code_server:literal_resolver(Module, Index)
                        end,
                        TypeResolver = fun(Index) -> code_server:type_resolver(Module, Index) end,
                        {StreamModule, Stream0} = jit:stream(jit_mmap_size(byte_size(Code))),
                        {BackendModule, BackendState0} = jit:backend(StreamModule, Stream0),
                        {LabelsCount, BackendState1} = jit:compile(
                            Code,
                            AtomResolver,
                            LiteralResolver,
                            TypeResolver,
                            BackendModule,
                            BackendState0
                        ),
                        Stream1 = BackendModule:stream(BackendState1),
                        Stream2 = StreamModule:flush(Stream1),
                        code_server:set_native_code(Module, LabelsCount, Stream2),
                        End = erlang:system_time(millisecond),
                        io:format("~B ms (bytecode: ~B bytes, native code: ~B bytes)\n", [
                            End - Start, byte_size(Code), BackendModule:offset(BackendState1)
                        ])
                    catch
                        T:V:S ->
                            io:format(
                                "===========================================================\nJust in time compilation of module ~p failed.\n~p:~p\n~p\n===========================================================\n",
                                [Module, T, V, S]
                            ),
                            exit(undef)
                    end
                end,
                [monitor, {atomvm_heap_growth, fibonacci}]
            ),
            receive
                {'DOWN', Ref, process, Pid, normal} -> ok;
                {'DOWN', Ref, process, Pid, undef} -> undef
            end;
        emu ->
            ok
    end.

%% @doc Estimate the required native code for a given bytecode size.
%% Currently, the only live stream backend that needs estimate is mmap
%% and it should not be passed a value too large to not slow down valgrind too
%% much during tests. A factor of 32 is more than enough, the largest observed
%% ratio is 21 for aarch64 and Elixir code. Also apply a minimum of 128 kb
%% which shouldn't affect valgrind too much.
%% jit_stream_flash and jit_stream_binary ignore the size parameter.
%% @return size in bytes
%% @param Size bytecode size
-spec jit_mmap_size(pos_integer()) -> pos_integer().
jit_mmap_size(Size) ->
    max(Size * 32, 128 * 1024).
