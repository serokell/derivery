-module(nix_ci_handler).

-export([init/2]).

init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Event = cowboy_req:header(<<"x-github-event">>, Req1),
    Resp = echo(Method, Event, Body, Req1),
    {ok, Resp, Opts}.

echo(<<"POST">>, <<"pull_request">>, Body, Req) ->
    {ok, Payload, _Rest} = json:decode(Body),
    #{<<"pull_request">> := #{<<"head">> := HEAD}} = Payload,
    #{<<"repo">> := #{<<"full_name">> := Name}} = HEAD,
    #{<<"sha">> := Rev} = HEAD,
    spawn(fun() -> build({Name, Rev}) end),
    cowboy_req:reply(202, Req);
echo(<<"POST">>, _, _, Req) ->
    cowboy_req:reply(200, Req);
echo(_, _, _, Req) ->
    cowboy_req:reply(405, Req).

build(Coord) ->
    nix_ci_github:status(Coord, <<"Building...">>, <<"pending">>),
    {Status, Output} = nix_ci_builder:build_tarball(nix_ci_github:source_archive(Coord)),
    Description = list_to_binary(lists:last(string:tokens(Output, "\n"))),
    URL = nix_ci_github:gist(iolist_to_binary(Output)),
    nix_ci_github:status(Coord, Description, encode_status(Status), URL).

encode_status(0) ->
    <<"success">>;
encode_status(_) ->
    <<"failure">>.
