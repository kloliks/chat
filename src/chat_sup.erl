-module(chat_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
           {storage, {storage, start_link, []}, permanent, 5000, worker, []},
           {chat_server, {chat_server, start_link, [storage]}, permanent, 5000, worker, []},
           {chat_bot, {chat_bot, start_link, []}, permanent, 5000, worker, []}
          ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
