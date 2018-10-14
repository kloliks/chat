-module(ws_h).

-export([init/2, terminate/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

-include("records.hrl").


init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.



websocket_init(State) ->
	{ok, State}.



channels_to_binary(Channels) ->
  lists:map(
    fun(#chinfo{name=Name, alias=Alias, unread=Unread}) ->
        #{name => list_to_binary(Name),
          alias => list_to_binary(Alias),
          unread => Unread
         }
    end,
    Channels
   ).



messages_to_binary(Messages) ->
  lists:map(
    fun(#message{time=Time, nickname=Nickname, text=Text}) ->
        #{time => Time,
          nickname => list_to_binary(Nickname),
          text => list_to_binary(Text)
         }
    end,
    Messages
   ).



reply_ok(Command) ->
  jsone:encode(#{
    code => 200,
    command => list_to_binary(Command)
   }).

reply_user_changed(Channels) ->
  jsone:encode(#{
    code => 300,
    command => <<"USER_CHANGED">>,
    channels => channels_to_binary(Channels)
   }).

reply_channel_changed(ChannelName) ->
  jsone:encode(#{
    code => 400,
    command => <<"CHANNEL_CHANGED">>,
    channel => list_to_binary(ChannelName)
   }).

reply_messages(Messages) ->
  jsone:encode(#{
    code => 500,
    command => <<"FETCH">>,
    messages => messages_to_binary(Messages)
   }).


reply_bad_nickname() ->
  jsone:encode(#{
    code => 1001,
    text => <<"Bad nickname">>
   }).
reply_user_exists() ->
  jsone:encode(#{
    code => 1002,
    text => <<"User exists">>
   }).
reply_user_not_exists() ->
  jsone:encode(#{
    code => 1003,
    text => <<"User not exists">>
   }).
reply_already_logged_in() ->
  jsone:encode(#{
    code => 1004,
    text => <<"Already logged in">>
   }).
reply_user_not_logged_in() ->
  jsone:encode(#{
    code => 1005,
    text => <<"User not logged in">>
   }).
reply_bad_channel_name() ->
  jsone:encode(#{
    code => 1006,
    text => <<"Bad channel name">>
   }).
reply_user_not_in_channel() ->
  jsone:encode(#{
    code => 1007,
    text => <<"User not in channel">>
   }).
 


websocket_handle({text, <<"SIGNUP,", Nickname/binary>>}, State) ->
  Nick = binary_to_list(Nickname),
  Msg = case chat_server:signup(Nick) of
          ok ->
            reply_ok("SIGNUP");
          {error, bad_nickname} ->
            reply_bad_nickname();
          {error, user_exists} ->
            reply_user_exists()
        end,
	{reply, {text, Msg}, State};


websocket_handle({text, <<"LOGIN,", Nickname/binary>>}, {none, _} = State) ->
  Nick = binary_to_list(Nickname),
  {Msg, NewState} = case chat_server:login(Nick) of
                      {error, user_not_exists} ->
                        {reply_user_not_exists(), State};
                      Channels ->
                        self() ! {user_changed, Channels},
                        {reply_ok("LOGIN"), {Nick, none}}
                    end,
	{reply, {text, Msg}, NewState};

websocket_handle({text, <<"LOGIN,", _Nickname/binary>>}, {_, _} = State) ->
  {reply, {text, reply_already_logged_in()}, State};


websocket_handle({text, _}, {none, _} = State) ->
  {reply, {text, reply_user_not_logged_in()}, State};


websocket_handle({text, <<"JOIN,", ChannelName/binary>>}, {Nickname, _} = State) ->
  ChName = binary_to_list(ChannelName),
  {Msg, NewState} = case chat_server:join(Nickname, ChName) of
                      {error, bad_channel_name} ->
                        {reply_bad_channel_name(), State};
                      {error, user_not_exists} ->
                        {reply_user_not_exists(), State};
                      RChannelName ->
                        {reply_ok("JOIN"), {Nickname, RChannelName}}
                    end,
  {reply, {text, Msg}, NewState};


websocket_handle({text, _}, {_, none} = State) ->
  {reply, {text, reply_user_not_in_channel()}, State};


websocket_handle({text, <<"SAY,", Text/binary>>}, {Nickname, ChannelName} = State) ->
  chat_server:say(Nickname, ChannelName, binary_to_list(Text)),
  {reply, {text, reply_ok("SAY")}, State};


websocket_handle({text, <<"LEAVE">>}, {Nickname, ChannelName} = State) ->
  chat_server:leave(Nickname, ChannelName),
  {reply, {text, reply_ok("LEAVE")}, State};


websocket_handle({text, <<"FETCH">>}, {Nickname, ChannelName} = State) ->
  Msg = reply_messages(chat_server:fetch(Nickname, ChannelName)),
  {reply, {text, Msg}, State};


websocket_handle(_Data, State) ->
	{ok, State}.



websocket_info({user_changed, Channels}, State) ->
  {reply, {text, reply_user_changed(Channels)}, State};

websocket_info({channel_changed, ChannelName}, State) ->
  {reply, {text, reply_channel_changed(ChannelName)}, State};

websocket_info(_Info, State) ->
	{ok, State}.



terminate(_Reason, _Req, {none, none}) ->
  ok;

terminate(_Reason, _Req, {Nickname, _}) ->
  chat_server:unlogin(Nickname),
  ok.
