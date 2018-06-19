-module(derivery_nix).
-export([build/2, fetch_git/3, import/1, multiple_outputs/1]).

fetch_git(URL, Ref, Rev) ->
    io_lib:format(<<"builtins.fetchGit {"
		    "url = ''~s'';"
		    "ref = ''~s'';"
		    "rev = ''~s'';"
		    "}">>,
		  [URL, Ref, Rev]).

import(Expr) ->
    io_lib:format(<<"import (~s)">>, [Expr]).

multiple_outputs(Expr) ->
    io_lib:format(<<"(~s).all">>, [Expr]).

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
    Report = build_with_args(Expr, [<<"--out-link">>, OutLink]),
    ok = file:change_time(os:getenv("HOME"), erlang:universaltime()),
    Report.

build_with_args(Expr, Args) ->
    Exec = os:find_executable("nix-build"),
    Port = erlang:open_port(
	     {spawn_executable, Exec},
	     [{args, [<<"-E">>, Expr, <<"--show-trace">>] ++ Args},
	      exit_status, stderr_to_stdout]),
    consume_port(Port).
