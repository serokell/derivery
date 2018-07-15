-module(derivery_nix).
-export([fetch_git/3, import/1, multiple_outputs/1, trace/2]).

fetch_git(URL, Ref, Rev) ->
    io_lib:format(<<"builtins.fetchGit {"
		    "url = ''~s'';"
		    "ref = ''~s'';"
		    "rev = ''~s'';"
		    "}">>,
		  [URL, Ref, Rev]).

import(Expr) ->
    io_lib:format(<<"import (~s)">>, [Expr]).

multiple_outputs(Expr) ->
    io_lib:format(<<"(~s).all">>, [Expr]).

trace(Msg, Expr) ->
    io_lib:format(<<"builtins.trace \"~s\" (~s)">>, [Msg, Expr]).
