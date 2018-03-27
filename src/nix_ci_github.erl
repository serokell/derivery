-module(nix_ci_github).

-export([gist/1, ssh_url/1, source_archive/1, status/3, status/4]).

token() -> os:getenv("NIX_CI_GITHUB_TOKEN").

request(Resource, Body) ->
    httpc:request(
      post,
      {"https://api.github.com" ++ Resource,
       [{"authorization", io_lib:format("token ~s", [token()])},
	{"user-agent", "nix-ci"}],
       "application/json",
       iolist_to_binary(json:encode(Body))},
      [], []).

gist(Data) ->
    gist(<<"nix-ci.log">>, Data).

gist(Name, Data) ->
    {ok, {_HTTP, _Headers, Payload}} = request("/gists", #{files => #{Name => #{content => Data}}}),
    {ok, #{<<"html_url">> := URL}, _Rest} = json:decode(Payload),
    URL.

ssh_url(Name) ->
    io_lib:format(<<"ssh://git@github.com:/~s.git">>, [Name]).

source_archive({Name, _Ref, Rev}) ->
    io_lib:format(<<"https://~s@github.com/~s/archive/~s.tar.gz">>, [token(), Name, Rev]).

status(Coordinates, Description, State) ->
    status(Coordinates, Description, State, null).

status({Name, _Ref, Rev}, Description, State, URL) ->
    {ok, _Response} = request(io_lib:format("/repos/~s/statuses/~s", [Name, Rev]),
			      #{context => <<"Nix CI">>,
				description => Description,
				state => State,
				target_url => URL}).
