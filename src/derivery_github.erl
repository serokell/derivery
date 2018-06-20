-module(derivery_github).
-export([commit_url/2, gist/1, is_file/3, status/3, status/4, tarball_url/2]).

request(Method, Resource) ->
    request(Method, Resource, #{}).

request(Method, Resource, Payload) ->
    {ok, Token} = application:get_env(derivery, github_token),
    {ok, ConnPid} = gun:open("api.github.com", 443),
    StreamRef = gun:request(ConnPid, Method, Resource, [
      {<<"authorization">>, [<<"token ">>, Token]},
      {<<"content-type">>, <<"application/json">>},
      {<<"user-agent">>, <<"Derivery">>}
    ], jsone:encode(Payload)),
    {response, _, Status, Headers} = gun:await(ConnPid, StreamRef),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    io:format(standard_error, "~s responded with ~B", [Resource, Status]),
    {Status, Headers, Body}.

commit_url(Name, Rev) ->
    io_lib:format(<<"https://github.com/~s/commit/~s">>, [Name, Rev]).

gist(Data) ->
    gist(<<"derivery.log">>, Data).

gist(Name, Data) ->
    {_, _, Body} = request(post, <<"/gists">>, #{files => #{Name => #{content => Data}}}),
    #{<<"html_url">> := URL} = jsone:decode(Body), URL.

is_file(Name, Rev, Path) ->
    {Status, _, _} = request(get, [<<"/repos/">>, Name, <<"/contents/">>, Path], #{ref => Rev}),
    Status == 200.

status(Name, Rev, State) ->
    status(Name, Rev, State, null).

status(Name, Rev, State, URL) ->
    request(post, [<<"/repos/">>, Name, <<"/statuses/">>, Rev],
	    #{context => <<"Derivery">>,
	      state => State,
	      target_url => URL}).

tarball_url(Name, Rev) ->
    {_, Headers, _} = request(get, [<<"/repos/">>, Name, <<"/tarball/">>, Rev]),
    #{<<"location">> := URL} = Headers, URL.
