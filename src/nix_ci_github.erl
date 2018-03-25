-module(nix_ci_github).

-export([gist/1, source_archive/1, status/3, status/4]).

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

source_archive({Name, Rev}) ->
    io_lib:format(<<"https://~s@github.com/~s/archive/~s.tar.gz">>, [token(), Name, Rev]).

status(Coord, Description, State) ->
    status(Coord, Description, State, "").

status({Name, Rev}, Description, State, URL) ->
    httpc:request(
      post,
      {io_lib:format("https://api.github.com/repos/~s/statuses/~s", [Name, Rev]),
       [{"authorization", io_lib:format("token ~s", [token()])},
	{"user-agent", "nix-ci"}],
       "application/json",
       iolist_to_binary(json:encode(#{context => <<"Nix CI">>,
				      description => Description,
				      state => State,
				      target_url => URL}))},
      [], []).
