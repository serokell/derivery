-module(derivery_hook).
-export([execute/2]).

execute(Req0, #{}) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Event = cowboy_req:header(<<"x-github-event">>, Req1),
    Signature = cowboy_req:header(<<"x-hub-signature">>, Req1),
    Response = handle(Event, Body, Req1, Signature),
    {ok, Response, #{}}.

handle(Event, Body, Req, Signature0) ->
    {ok, Secret} = application:get_env(derivery, github_secret),
    Signature1 = encode_signature(crypto:hmac(sha, Secret, Body)),
    if Signature0 == Signature1 ->
	    handle_trusted(Event, Body, Req);
       true ->
	    cowboy_req:reply(400, Req)
    end.

binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) ||
		      X <- binary_to_list(Bin)]).

encode_signature(Bin) ->
    list_to_binary("sha1=" ++ binary_to_hex(Bin)).

handle_trusted(<<"pull_request">>, Body, Req) ->
    #{<<"pull_request">> := #{<<"head">> := HEAD}} = jsone:decode(Body),
    #{<<"repo">> := #{<<"full_name">> := Name}} = HEAD,
    #{<<"ref">> := Ref} = HEAD,
    #{<<"sha">> := Rev} = HEAD,
    spawn(fun() -> build(Name, Ref, Rev) end),
    cowboy_req:reply(202, Req);
handle_trusted(_, _, Req) ->
    cowboy_req:reply(200, Req).

build(Name, Ref, Rev) ->
    derivery_github:status(Name, Rev, <<"pending">>),
    Expr = derivery_nix:git_expression(derivery_github:ssh_url(Name), Ref, Rev),
    {Status, Output} = derivery_nix:build(Expr),
    GistURL = derivery_github:gist(iolist_to_binary(Output)),
    derivery_github:status(Name, Rev, encode_status(Status), GistURL).

encode_status(0) ->
    <<"success">>;
encode_status(_) ->
    <<"failure">>.
