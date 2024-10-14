%
% This file is part of AtomVM.
%
% Copyright 2020-2021 Fred Dushin <fred@dushin.net>
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

-module(mkimage).

-export([main/1]).

main(Argv) ->
    erlang:halt(do_main(Argv)).

%% @private
do_main(Argv) ->
    {Opts, _Args} = parse_args(Argv),
    case maps:get(help, Opts, false) of
        true ->
            print_help(),
            0;
        _ ->
            case maps:get(root_dir, Opts, undefined) of
                undefined ->
                    print_help(
                        "root_dir option is required and should be the root directory of the AtomVM checkout"
                    ),
                    255;
                RootDir ->
                    try
                        Config = load_config(maps:get(config, Opts, "mkimage.config")),
                        BuildDir = get_build_dir(Opts, RootDir),
                        BootFile = BuildDir ++ "/libs/esp32boot/esp32boot.avm",
                        mkimage(
                            RootDir,
                            BuildDir,
                            maps:get(boot, Opts, BootFile),
                            maps:get(out, Opts, "atomvm.img"),
                            maps:get(segments, Config)
                        ),
                        0
                    catch
                        _:Exception:Stacktrace ->
                            io:format("Stacktrace: ~p~n", [Stacktrace]),
                            print_help(io_lib:format("~s", [to_string(Exception)])),
                            255
                    end
            end
    end.

%% @private
parse_args(Argv) ->
    parse_args(Argv, {#{}, []}).

%% @private
parse_args([], {Opts, Args}) ->
    {Opts, lists:reverse(Args)};
parse_args(["--boot", Path | T], {Opts, Args}) ->
    parse_args(T, {Opts#{boot => Path}, Args});
parse_args(["--out", Path | T], {Opts, Args}) ->
    parse_args(T, {Opts#{out => Path}, Args});
parse_args(["--root_dir", Path | T], {Opts, Args}) ->
    parse_args(T, {Opts#{root_dir => Path}, Args});
parse_args(["--config", Path | T], {Opts, Args}) ->
    parse_args(T, {Opts#{config => Path}, Args});
parse_args(["--help" | T], {Opts, Args}) ->
    parse_args(T, {Opts#{help => true}, Args});
parse_args([H | T], {Opts, Args}) ->
    parse_args(T, {Opts, [H | Args]}).

%% @private
print_help(Msg) ->
    io:format("%%~n"),
    io:format("%% ~s~n", [Msg]),
    io:format("%%~n"),
    print_help().

%% @private
print_help() ->
    io:format(
        "Syntax:~n"
        "    escript mkimage.erl <options>~n"
        "~n"
        "The following options are supported:"
        "~n"
        "    * --root_dir <path>    Path to the root directory of the AtomVM git checkout~n"
        "    * --boot <path>        Path to a esp32boot.avm file~n"
        "    * --build_dir <path>   Path to the AtomVM build directory (defaults to root_dir/build, if unspecifeid)~n"
        "    * --out <path>         Output path for AtomVM image file~n"
        "    * --config <path>      Path to mkimage configuration file~n"
        "    * --help               Print this help"
        "~n"
    ).

%% @private
to_string(S) when is_list(S) ->
    S;
to_string(P) ->
    io_lib:format("~p", [P]).

%% @private
load_config(ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, [Contents]} ->
            Contents;
        {error, Reason} ->
            throw(io_lib:format("Unable to load ~s for reason ~p", [ConfigFile, Reason]))
    end.

%% @private
get_build_dir(Opts, RootDir) ->
    case maps:get(build_dir, Opts, undefined) of
        undefined ->
            RootDir ++ "/build";
        BuildDir ->
            BuildDir
    end.

%% @private
mkimage(RootDir, BuildDir, BootFile, OutputFile, Segments) ->
    io:format("Writing output to ~s~n", [OutputFile]),
    io:format("boot file is ~s~n", [BootFile]),
    io:format("=============================================~n"),
    case file:open(OutputFile, [write, binary]) of
        {ok, Fout} ->
            lists:foldl(
                fun(Segment, PrevOffset) ->
                    SegmentOffset = from_hex(maps:get(offset, Segment)),
                    case PrevOffset of
                        undefined ->
                            no_padding;
                        _ ->
                            case SegmentOffset > PrevOffset of
                                true ->
                                    Padding = [
                                        16#FF
                                     || _ <- lists:seq(1, SegmentOffset - PrevOffset)
                                    ],
                                    io:format("Padding ~p bytes~n", [SegmentOffset - PrevOffset]),
                                    file:write(Fout, Padding);
                                false ->
                                    throw(
                                        io_lib:format(
                                            "Error: insufficient space for segment ~p.  Over by: ~p bytes~n",
                                            [
                                                maps:get(name, Segment), PrevOffset - SegmentOffset
                                            ]
                                        )
                                    )
                            end
                    end,
                    SegmentPaths = [
                        replace(
                            "BUILD_DIR",
                            BuildDir,
                            replace(
                                "BOOT_FILE", BootFile, replace("ROOT_DIR", RootDir, SegmentPath)
                            )
                        )
                     || SegmentPath <- maps:get(path, Segment)
                    ],
                    case try_read(SegmentPaths) of
                        {ok, Data} ->
                            file:write(Fout, Data),
                            io:format("Wrote ~s (~p bytes) at offset ~s (~p)~n", [
                                maps:get(name, Segment),
                                byte_size(Data),
                                maps:get(offset, Segment),
                                SegmentOffset
                            ]),
                            SegmentOffset + byte_size(Data);
                        {error, Reason} ->
                            Fmt =
                                "Failed to read file ~p  Reason: ~p."
                                "  Note that a full build is required before running this command.",
                            throw(io_lib:format(Fmt, [SegmentPaths, Reason]))
                    end
                end,
                undefined,
                Segments
            );
        {error, Reason} ->
            throw(io_lib:format("Failed to open ~s for writing.  Reason: ~p", [OutputFile, Reason]))
    end.

%% @private
try_read([]) ->
    {error, not_found};
try_read([Path | Rest]) ->
    case file:read_file(Path) of
        {ok, Data} ->
            {ok, Data};
        {error, _Reason} ->
            try_read(Rest)
    end.

%% @private
from_hex([$0, $x | Bits]) ->
    erlang:list_to_integer(Bits, 16).

%% @private
replace(VariableName, Value, String) ->
    string:replace(String, io_lib:format("${~s}", [VariableName]), Value),
    string:replace(String, io_lib:format("$[~s]", [VariableName]), Value).
