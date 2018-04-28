-module(derivery_app).
-export([start/2, stop/1]).

-behaviour(application).

start(_Type, _Args) ->
    {ok, Port} = application:get_env(derivery, port),
    {ok, _Pid} = cowboy:start_clear(derivery, [{port, Port}], #{middlewares => [derivery_hook]}),
    derivery_sup:start_link().

stop(_State) ->
    ok.
