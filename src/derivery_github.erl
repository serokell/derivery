-module(derivery_github).
-export([commit_url/2, gist/1, ssh_url/1, status/3, status/4]).

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

commit_url(Name, Rev) ->
    io_lib:format(<<"https://github.com/~s/commit/~s">>, [Name, Rev]).

gist(Data) ->
    gist(<<"derivery.log">>, Data).

gist(Name, Data) ->
    Payload = request(<<"/gists">>,
		      #{files => #{Name => #{content => Data}}}),
    #{<<"html_url">> := URL} = Payload, URL.

ssh_url(Name) ->
    io_lib:format(<<"ssh://git@github.com:/~s.git">>, [Name]).

status(Name, Rev, State) ->
    status(Name, Rev, State, null).

status(Name, Rev, State, URL) ->
    request([<<"/repos/">>, Name, <<"/statuses/">>, Rev],
	    #{context => <<"Derivery">>,
	      state => State,
	      target_url => URL}).
