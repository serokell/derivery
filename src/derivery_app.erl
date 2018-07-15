-module(derivery_app).
-export([start/2, stop/1]).

-behaviour(application).

start(_Type, _Args) ->
    {ok, Opts} = application:get_env(derivery, ranch_tcp_opts),
    {ok, _Pid} = cowboy:start_clear(derivery, Opts, #{middlewares => [derivery_hook]}).

stop(_State) ->
    ok.
