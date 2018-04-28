-module(derivery_github).
-export([gist/1, ssh_url/1, source_archive/1, status/3, status/4]).

%% TODO: https://github.com/ninenines/gun
request(Resource, Body) ->
    {ok, Token} = application:get_env(derivery, github_token),
    httpc:request(
      post,
      {"https://api.github.com" ++ Resource,
       [{"authorization", io_lib:format("token ~s", [Token])},
	{"user-agent", "Derivery"}],
       "application/json",
       iolist_to_binary(jsone:encode(Body))},
      [], []).

gist(Data) ->
    gist(<<"derivery.log">>, Data).

gist(Name, Data) ->
    case request("/gists", #{files => #{Name => #{content => Data}}}) of
	{ok, {_HTTP, _Headers, Payload}} ->
	    #{<<"html_url">> := URL} = jsone:decode(iolist_to_binary(Payload)),
	    URL;
	_ ->
	    null
    end.

ssh_url(Name) ->
    io_lib:format(<<"ssh://git@github.com:/~s.git">>, [Name]).

source_archive({Name, _Ref, Rev}) ->
    {ok, Token} = application:get_env(derivery, github_token),
    io_lib:format(<<"https://~s@github.com/~s/archive/~s.tar.gz">>, [Token, Name, Rev]).

status(Coordinates, Description, State) ->
    status(Coordinates, Description, State, null).

status({Name, _Ref, Rev}, Description, State, URL) ->
    {ok, _Response} = request(io_lib:format("/repos/~s/statuses/~s", [Name, Rev]),
			      #{context => <<"Derivery">>,
				description => Description,
				state => State,
				target_url => URL}).
