%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(blackjack_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, websocket, "index.html"}},
			{"/websocket", ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}},
			{"/css/[...]", cowboy_static, {priv_dir, websocket, "css"}},
			{"/images/[...]", cowboy_static, {priv_dir, websocket, "images"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	websocket_sup:start_link().

stop(_State) ->
	ok.
