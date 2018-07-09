-module(derivery_hook).
-export([execute/2]).

execute(Req0, #{}) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    {ok, authenticate(Body, Req1), #{}}.

authenticate(Body, Req) ->
    {ok, Secret} = application:get_env(derivery, github_secret),
    Signature0 = cowboy_req:header(<<"x-hub-signature">>, Req),
    Signature1 = encode_signature(crypto:hmac(sha, Secret, Body)),
    if Signature0 == Signature1 ->
	    Event = cowboy_req:header(<<"x-github-event">>, Req),
	    handle(Event, jsone:decode(Body), Req);
       true ->
            io:format(standard_error,
		      <<"rejected payload (got ~s, expected ~s): ~s">>,
		      [Signature0, Signature1, Body]),
	    cowboy_req:reply(400, Req)
    end.

binary_to_hex(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [X]) ||
		      X <- binary_to_list(Bin)]).

encode_signature(Bin) ->
    list_to_binary("sha1=" ++ binary_to_hex(Bin)).

ref_to_branch(<<"refs/heads/", Branch/binary>>) -> Branch.

handle(<<"pull_request">>, Payload, Req) ->
    #{<<"pull_request">> := #{<<"head">> := HEAD}} = Payload,
    #{<<"repo">> := #{<<"full_name">> := Name},
      <<"sha">> := Rev} = HEAD,
    spawn(fun() -> build(Name, Rev) end),
    cowboy_req:reply(202, Req);
handle(<<"push">>, Payload, Req) ->
    #{<<"repository">> := #{<<"full_name">> := Name},
      <<"ref">> := Ref,
      <<"after">> := Rev} = Payload,
    OutLink = filename:join([os:getenv("HOME"), "github.com", Name, ref_to_branch(Ref)]),
    spawn(fun() -> build(Name, Rev, OutLink) end),
    cowboy_req:reply(202, Req);
handle(_, _, Req) ->
    cowboy_req:reply(200, Req).

build(Name, Rev) ->
	build(Name, Rev, none).

build(Name, Rev, OutLink) ->
    io:format(standard_error, <<"building ~s@~s">>, [Name, Rev]),
    case derivery_github:is_file(Name, Rev, <<"default.nix">>) of
        true ->
            derivery_github:status(Name, Rev, <<"pending">>),
            Src = derivery_nix:fetch_tarball(derivery_github:tarball_url(Name, Rev)),
            Expr = derivery_nix:trace(derivery_github:commit_url(Name, Rev), derivery_nix:multiple_outputs(derivery_nix:import(Src))),
            {Status, Output} = derivery_nix:build(Expr, OutLink),
            GistURL = derivery_github:gist(iolist_to_binary(Output)),
            derivery_github:status(Name, Rev, encode_status(Status), GistURL)
    end.

encode_status(0) ->
    <<"success">>;
encode_status(_) ->
    <<"failure">>.
