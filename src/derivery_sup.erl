-module(derivery_sup).
-export([start_link/0, init/1]).

-behaviour(supervisor).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 0, 1}, []}}.
