-module(storage).
-behavior(gen_server).

-export([load/0, save/1]).
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([start/0]).


load() ->
  gen_server:call(?MODULE, {load}).

save(State) ->
  gen_server:call(?MODULE, {save, State}).

stop() ->
  gen_server:cast(?MODULE, stop).



start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, none, []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).



init(State) ->
  {ok, State}.



terminate(_Reason, _State) ->
  ok.



handle_cast(stop, State) ->
  {stop, normal, State}.



handle_call({load}, _From, State) ->
  {reply, State, State};

handle_call({save, State}, _From, _State) ->
  {reply, State, State}.
