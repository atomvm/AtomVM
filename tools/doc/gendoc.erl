-module(gendoc).

-export([main/1]).

main([LibraryName, SrcDir, TgtDir]) ->
    {ok, AllFiles} = file:list_dir(SrcDir),
    ErlFiles = [SrcDir ++ "/" ++ F || F <- AllFiles, ends_with(F, ".erl")],
    ok = edoc:run(
        ErlFiles, [
            {source_path, SrcDir},
            {dir, TgtDir},
            {application, LibraryName}
        ]
    ),
    io:format("Generated documentation for application ~p using source files ~p into ~s~n", [LibraryName, ErlFiles, TgtDir]).


ends_with(Str, Suffix) ->
    string:find(Str, Suffix, trailing) =:= Suffix.
