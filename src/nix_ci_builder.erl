-module(nix_ci_builder).

-export([build/1, build_tarball/1]).

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
				  [{args, [<<"-E">>, Expr]}, exit_status])).

build_tarball(URL) ->
    build(tarball_expression(URL)).
