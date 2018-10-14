-module(chat_server).
-behavior(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([signup/1, login/1, unlogin/1, join/2, leave/2, invite/2, say/3, broadcast/2, fetch/2]).

-include("records.hrl").


signup(Nickname) ->
  gen_server:call(?MODULE, {signup, Nickname}).

login(Nickname) ->
  gen_server:call(?MODULE, {login, Nickname}).

unlogin(Nickname) ->
  gen_server:call(?MODULE, {unlogin, Nickname}).

join(Nickname, ChannelName) ->
  gen_server:call(?MODULE, {join, Nickname, ChannelName}).

leave(Nickname, ChannelName) ->
  gen_server:call(?MODULE, {leave, Nickname, ChannelName}).

invite(Nickname, ChannelName) ->
  gen_server:call(?MODULE, {invite, Nickname, ChannelName}).

say(Nickname, ChannelName, Text) ->
  gen_server:call(?MODULE, {say, Nickname, ChannelName, Text}).

broadcast(Name, Text) ->
  gen_server:call(?MODULE, {broadcast, Name, Text}).

fetch(Nickname, ChannelName) ->
  gen_server:call(?MODULE, {fetch, Nickname, ChannelName}).


start_link(Storage) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Storage, []).



stop() ->
  gen_server:cast(?MODULE, stop).



init(Storage) ->
  case Storage:load() of
    none ->
      Storage:save(#state{});
    _ ->
      ok
  end,
  {ok, Storage}.



terminate(_Reason, _Storage) ->
  ok.



handle_cast(stop, Storage) ->
  {stop, normal, Storage}.



user_changed(#user{sessions=Sessions, channels=Channels}) ->
  Send = fun(Pid) ->
             Pid ! {user_changed, Channels}
         end,
  lists:foreach(Send, Sessions).



join(false, _, _, State) ->
  {{error, user_not_exists}, State};
join(User, false, ChannelName, State) ->
  join(User, #channel{name=ChannelName}, ChannelName, State);
join(User, Channel, Alias, State) ->
   #user{name=Nickname, channels=UserChannels} = User,
   #channel{name=ChannelName, users=ChannelUsers} = Channel,
   #state{users=Users, channels=Channels} = State,
   case lists:member(Nickname, ChannelUsers) of
     true ->
       {ChannelName, State};
     false ->
       NewChannel = Channel#channel{users=[Nickname|ChannelUsers]},
       NewChannels = [NewChannel|lists:keydelete(ChannelName, #channel.name, Channels)],
       NewUser = User#user{channels=[#chinfo{channel_id=ChannelName, name=Alias}|UserChannels]},
       NewUsers = lists:keyreplace(Nickname, #user.name, Users, NewUser),
       user_changed(NewUser),
       {ChannelName, State#state{users=NewUsers, channels=NewChannels}}
   end.


say(Nickname, Channel, Text, State) ->
  #state{users=Users, channels=Channels} = State,
  #channel{name=ChannelName, users=ChannelUsers, messages=Messages} = Channel,
  NewMessages = [#message{time=os:system_time(), nickname=Nickname, text=Text}|Messages],
  NewChannel = Channel#channel{messages=NewMessages},
  NewChannels = lists:keyreplace(ChannelName, #channel.name, Channels, NewChannel),

  NewUsers = lists:foldl(
               fun(Nick, UsersAcc) ->
                   ChUser = lists:keyfind(Nick, #user.name, UsersAcc),
                   #user{channels=ChUserChannels, sessions=Sessions} = ChUser,

                   ChannelInfo = lists:keyfind(
                                   ChannelName,
                                   #chinfo.channel_id,
                                   ChUserChannels),
                   #chinfo{unread=Unread, name=ChName} = ChannelInfo,

                   NewChannelInfo = ChannelInfo#chinfo{unread=Unread+1},
                   NewUserChannels = lists:keyreplace(
                                       ChannelName,
                                       #chinfo.channel_id,
                                       ChUserChannels,
                                       NewChannelInfo),
                   NewUser = ChUser#user{channels=NewUserChannels},

                   Send = fun(Pid) ->
                              Pid ! {user_changed, NewUserChannels},
                              Pid ! {channel_changed, ChName}
                          end,
                   lists:foreach(Send, Sessions),
                   lists:keyreplace(Nick, #user.name, UsersAcc, NewUser)
               end,
               Users,
               ChannelUsers),
  NewState = State#state{users=NewUsers, channels=NewChannels},
  {ok, NewState}.



handle_call(Msg, From, Storage) ->
  {Reply, State} = h_call(Msg, From, Storage:load()),
  Storage:save(State),
  {reply, Reply, Storage}.



h_call({signup, Nickname}, _From, #state{users=Users} = State) ->
  {ok, Mp} = re:compile("^[A-Za-z0-9]+$"),
  case re:run(Nickname, Mp) of
    nomatch ->
      {{error, bad_nickname}, State};
    _ ->
      case lists:keyfind(Nickname, #user.name, Users) of
        false ->
          NewUsers = [#user{name=Nickname}|Users],
          {ok, State#state{users=NewUsers}};
        _User ->
          {{error, user_exists}, State}
      end
  end;


h_call({login, Nickname}, {Pid, _}, #state{users=Users} = State) ->
  case lists:keyfind(Nickname, #user.name, Users) of
    false ->
      {{error, user_not_exists}, State};
    #user{channels=Channels, sessions=Sessions} = User ->
      case lists:member(Pid, Sessions) of
        true ->
          {Channels, State};
        false ->
          NewUser = User#user{sessions=[Pid|Sessions]},
          NewUsers = lists:keyreplace(Nickname, #user.name, Users, NewUser),
          {Channels, State#state{users=NewUsers}}
      end
  end;


h_call({unlogin, Nickname}, {Pid, _}, #state{users=Users} = State) ->
  case lists:keyfind(Nickname, #user.name, Users) of
    false ->
      {ok, State};
    #user{sessions=Sessions} = User ->
      NewUser = User#user{sessions=lists:delete(Pid, Sessions)},
      NewUsers = lists:keyreplace(Nickname, #user.name, Users, NewUser),
      {ok, State#state{users=NewUsers}}
  end;


h_call({join, Nickname1, [$@|Nickname2]}, _From, #state{users=Users, channels=Channels} = State) ->
  User1 = lists:keyfind(Nickname1, #user.name, Users),
  User2 = lists:keyfind(Nickname2, #user.name, Users),
  ChannelName = case Nickname1 < Nickname2 of
                   true ->
                     lists:flatten([Nickname1, $@, Nickname2]);
                   false ->
                     lists:flatten([Nickname2, $@, Nickname1])
                 end,
  case {User1, User2, lists:keyfind(ChannelName, #channel.name, Channels)} of
    {false, _, _} ->
      {{error, user_not_exists}, State};
    {_, false, _} ->
      {{error, user_not_exists}, State};
    {_, _, false} ->
      Channel = #channel{name=ChannelName},
      {_, #state{channels=NewChannels} = NewState} = join(User2, Channel, [$@|Nickname1], State),
      join(User1, lists:keyfind(ChannelName, #channel.name, NewChannels), [$@|Nickname2], NewState);
    {_, _, Channel} ->
      join(User1, Channel, [$@|Nickname2], State)
  end;

h_call({join, _, [$#|[]]}, _From, State) ->
  {{error, bad_channel_name}, State};

h_call({join, Nickname, [$#|_] = ChannelName}, _From, #state{users=Users, channels=Channels} = State) ->
  User = lists:keyfind(Nickname, #user.name, Users),
  Channel = lists:keyfind(ChannelName, #channel.name, Channels),
  join(User, Channel, ChannelName, State);

h_call({join, _, _}, _From, State) ->
  {{error, bad_channel_name}, State};


h_call({leave, Nickname, ChannelName}, _From, #state{users=Users, channels=Channels} = State) ->
  User = lists:keyfind(Nickname, #user.name, Users),
  Channel = lists:keyfind(ChannelName, #channel.name, Channels),
  case {User, Channel} of
    {false, _} ->
      {ok, State};
    {_, false} ->
      {ok, State};
    _ ->
      #user{channels=UserChannels} = User,
      #channel{users=ChannelUsers} = Channel,
      case lists:member(Nickname, ChannelUsers) of
        false ->
          {ok, State};
        true ->
          NewUserChannels = lists:keydelete(ChannelName, #chinfo.channel_id, UserChannels),
          NewUser = User#user{channels=NewUserChannels},
          NewUsers = lists:keyreplace(Nickname, #user.name, Users, NewUser),
          NewChannels = case lists:delete(Nickname, ChannelUsers) of
                          [] ->
                            lists:keydelete(ChannelName, #channel.name, Channels);
                          NewChannelUsers ->
                            NewChannel = Channel#channel{users=NewChannelUsers},
                            lists:keyreplace(ChannelName, #channel.name, Channels, NewChannel)
                        end,
          user_changed(NewUser),
          {ok, State#state{users=NewUsers, channels=NewChannels}}
      end
  end;


h_call({invite, _, [$#|[]]}, _From, State) ->
  {{error, bad_channel_name}, State};

h_call({invite, Nickname, [$#|_] = ChannelName}, _From, #state{users=Users, channels=Channels} = State) ->
  User = lists:keyfind(Nickname, #user.name, Users),
  case lists:keyfind(ChannelName, #channel.name, Channels) of
    false ->
      {{error, channel_not_exists}, State};
    Channel ->
      join(User, Channel, ChannelName, State)
  end;

h_call({invite, _, _}, _From, State) ->
  {{error, bad_channel_name}, State};


h_call({say, _, _, []}, _From, State) ->
  {ok, State};

h_call({say, Nickname, ChannelName, Text}, _From, #state{users=Users, channels=Channels} = State) ->
  User = lists:keyfind(Nickname, #user.name, Users),
  Channel = lists:keyfind(ChannelName, #channel.name, Channels),
  case {User, Channel} of
    {false, _} ->
      {{error, user_not_exists}, State};
    {_, false} ->
      {{error, channel_not_exists}, State};
    {_, _} ->
      #user{channels=UserChannels} = User,
      case lists:keyfind(ChannelName, #chinfo.channel_id, UserChannels) of
        false ->
          {{error, user_not_invited_to_channel}, State};
        _ ->
          say(Nickname, Channel, Text, State)
      end
  end;


h_call({broadcast, Name, Text}, _From, #state{channels=Channels} = State) ->
  NewState = lists:foldl(
               fun(Channel, StateAcc) ->
                   {ok, NewStateAcc} = say(Name, Channel, Text, StateAcc),
                   NewStateAcc
               end,
               State,
               Channels
              ),
  {ok, NewState};


h_call({fetch, Nickname, ChannelName}, _From, #state{channels=Channels, users=Users} = State) ->
  User = lists:keyfind(Nickname, #user.name, Users),
  Channel = lists:keyfind(ChannelName, #channel.name, Channels),
  case {User, Channel} of
    {false, _} ->
      {{error, user_not_exists}, State};
    {_, false} ->
      {{error, channel_not_exists}, State};
    {_, _} ->
      #user{channels=UserChannels} = User,
      #channel{messages=Messages} = Channel,
      ChannelInfo = lists:keyfind(ChannelName, #chinfo.channel_id, UserChannels),

      NewChannelInfo = ChannelInfo#chinfo{unread=0},
      NewUserChannels = lists:keyreplace(ChannelName, #chinfo.channel_id, UserChannels, NewChannelInfo),
      NewUser = User#user{channels=NewUserChannels},
      NewUsers = lists:keyreplace(Nickname, #user.name, Users, NewUser),

      user_changed(NewUser),
      {Messages, State#state{users=NewUsers}}
  end.
