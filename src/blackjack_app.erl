%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(blackjack_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	application:ensure_all_started(cowboy),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, blackjack, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, blackjack, "static"}},
			{"/css/[...]", cowboy_static, {priv_dir, blackjack, "css"}},
			{"/images/[...]", cowboy_static, {priv_dir, blackjack, "images"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	blackjack_sup:start_link().

stop(_State) ->
	ok.
