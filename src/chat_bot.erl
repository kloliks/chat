-module(chat_bot).
-behavior(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, handle_info/2, handle_call/3, handle_cast/2]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).



stop() ->
  gen_server:cast(?MODULE, stop).



init(State) ->
  erlang:start_timer(rand:uniform(30000) + 20000, self(), []),
  {ok, State}.



terminate(_Reason, _State) ->
  ok.



handle_cast(stop, State) ->
  {stop, normal, State}.



handle_call(_Msg, _From, State) ->
  {reply, ok, State}.



handle_info({timeout, _Ref, _Msg}, State) ->
  {{Year, Mon, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
  Text = io_lib:format(
           "Server time: ~2..0B.~2..0B.~B ~2..0B:~2..0B:~2..0B",
           [Day, Mon, Year, Hour, Min, Sec]
          ),
  chat_server:broadcast("system", Text),
  erlang:start_timer(rand:uniform(30000) + 20000, self(), []),
  {noreply, State}.
