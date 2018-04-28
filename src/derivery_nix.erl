-module(derivery_nix).
-export([build/1, fetch_git/3, fetch_tarball/1]).

fetch_git(URL, Ref, Rev) ->
    io_lib:format(<<"import (builtins.fetchGit { url = \"~s\"; ref = \"~s\"; rev = \"~s\"; })">>,
		  [URL, Ref, Rev]).

fetch_tarball(URL) ->
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
				  [{args, [<<"-E">>, Expr, <<"--no-out-link">>, <<"--show-trace">>]},
				   exit_status, stderr_to_stdout])).
