-module(derivery_nix_build).
-export([build_cached/2]).

consume_port(Port) ->
    consume_port(Port, []).

consume_port(Port, Log) ->
    receive
        {Port, {data, Data}} ->
            consume_port(Port, [Log|Data]);
        {Port, {exit_status, Status}} ->
            {Status, Log}
    end.

status_to_term(0) -> ok;
status_to_term(_) -> error.

execute(Name, Args) ->
    Exec = os:find_executable(Name),
    Port = erlang:open_port({spawn_executable, Exec},
	     [{args, Args}, exit_status, stderr_to_stdout]),
    {Status, Log} = consume_port(Port),
    {status_to_term(Status), Log}.

build_with_args(Expr, Args) ->
    execute("nix-build", [<<"--expr">>, Expr, <<"--show-trace">>] ++ Args).

retrieve_log(Path) ->
    execute("nix-store", [<<"--read-log">>, Path]).

cached_path(Expr) ->
    {Status, Log} = build_with_args(Expr, [<<"--max-jobs">>, <<"0">>, <<"--no-out-link">>]),
    case Status of
        ok ->
            string:trim(Log);
        error ->
            not_found
    end.

build(Expr, no_out_link) ->
    build_with_args(Expr, [<<"--no-out-link">>]);
build(Expr, OutLink) ->
    Report = build_with_args(Expr, [<<"--out-link">>, OutLink]),
    file:change_time(os:getenv("HOME"), erlang:universaltime()),
    Report.

build_cached(Expr, OutLink) ->
    case cached_path(Expr) of
        not_found ->
            build(Expr, OutLink);
	Path ->
	    retrieve_log(Path)
    end.
