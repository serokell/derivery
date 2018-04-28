-module(derivery_github).
-export([gist/1, ssh_url/1, archive_url/2, status/3, status/4]).

request(Resource, ReqBody) ->
    {ok, Token} = application:get_env(derivery, github_token),
    {ok, ConnPid} = gun:open("api.github.com", 443),
    StreamRef = gun:post(ConnPid, Resource, [
      {<<"authorization">>, [<<"token ">>, Token]},
      {<<"content-type">>, <<"application/json">>},
      {<<"user-agent">>, <<"Derivery">>}
    ], jsone:encode(ReqBody)),
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
    jsone:decode(RespBody).

gist(Data) ->
    gist(<<"derivery.log">>, Data).

gist(Name, Data) ->
    Payload = request(<<"/gists">>,
		      #{files => #{Name => #{content => Data}}}),
    #{<<"html_url">> := URL} = Payload, URL.

ssh_url(Name) ->
    io_lib:format(<<"ssh://git@github.com:/~s.git">>, [Name]).

archive_url(Name, Rev) ->
    {ok, Token} = application:get_env(derivery, github_token),
    io_lib:format(<<"https://~s@github.com/~s/archive/~s.tar.gz">>,
		  [Token, Name, Rev]).

status(Name, Rev, State) ->
    status(Name, Rev, State, null).

status(Name, Rev, State, URL) ->
    request([<<"/repos/">>, Name, <<"/statuses/">>, Rev],
	    #{context => <<"Derivery">>,
	      state => State,
	      target_url => URL}).
