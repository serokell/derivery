-module(nix_ci_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", nix_ci_handler, []}]}]),
    {ok, _} = cowboy:start_clear(http, [{port, 50493}], #{env => #{dispatch => Dispatch}}),
    nix_ci_sup:start_link().

stop(_State) ->
    ok.
