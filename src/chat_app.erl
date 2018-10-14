-module(chat_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, chat, "index.html"}},
			{"/static/[...]", cowboy_static, {priv_dir, chat, "static"}},
			{"/websocket", ws_h, {none, none}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	chat_sup:start_link().

stop(_State) ->
	ok.
