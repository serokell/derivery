-module(derivery_nix).
-export([build/2, fetch_git/3, fetch_tarball/1, import/1]).

fetch_git(URL, Ref, Rev) ->
    io_lib:format(<<"builtins.fetchGit {"
		    "url = ''~s'';"
		    "ref = ''~s'';"
		    "rev = ''~s'';"
		    "}">>,
		  [URL, Ref, Rev]).

fetch_tarball(URL) ->
    io_lib:format(<<"builtins.fetchTarball ~s">>, [URL]).

import(Expr) ->
    io_lib:format(<<"import (~s)">>, [Expr]).

consume_port(Port) ->
    consume_port(Port, []).

consume_port(Port, Output) ->
    receive
	{Port, {data, Bytes}} ->
	    consume_port(Port, [Output|Bytes]);
	{Port, {exit_status, Status}} ->
	    {Status, Output}
    end.

build(Expr, none) ->
    build_with_args(Expr, [<<"--no-out-link">>]);
build(Expr, OutLink) ->
    ok = filelib:ensure_dir(filename:dirname(OutLink)),
    build_with_args(Expr, [<<"--out-link">>, OutLink]).

build_with_args(Expr, Args) ->
    Exec = os:find_executable("nix-build"),
    Port = erlang:open_port(
	     {spawn_executable, Exec},
	     [{args, [<<"-E">>, Expr, <<"--show-trace">>] ++ Args},
	      exit_status, stderr_to_stdout]),
    consume_port(Port).
