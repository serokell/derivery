-module(nix_ci_builder).

-export([build/1, git_expression/3, tarball_expression/1]).

git_expression(URL, Ref, Rev) ->
    io_lib:format(<<"import (builtins.fetchGit { url = \"~s\"; ref = \"~s\"; rev = \"~s\"; })">>,
		  [URL, Ref, Rev]).

tarball_expression(URL) ->
    io_lib:format(<<"import (builtins.fetchTarball ~s)">>, [URL]).

consume_port(Port) ->
    consume_port(Port, []).

consume_port(Port, Output) ->
    receive
	{Port, {data, Bytes}} ->
	    consume_port(Port, [Output|Bytes]);
	{Port, {exit_status, Status}} ->
	    {Status, Output}
	end.

build(Expr) ->
    consume_port(erlang:open_port({spawn_executable, os:find_executable("nix-build")},
				  [{args, [<<"-E">>, Expr, "--no-out-link"]},
				   exit_status, stderr_to_stdout])).
