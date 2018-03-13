-module (dispatcher).
-behaviour (gen_server).
-export([start_link/0, query/0, create/0, join/1, left/0, ask/0, stick/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% []{pid of room, peoplenumber}], dict of websocket pid => room pid
query() ->
  gen_server:cast(?MODULE, {query, self()}).

create() ->
  gen_server:cast(?MODULE, {create, self()}).

join(Room) ->
  gen_server:cast(?MODULE, {join, self(), Room}).

left() ->
  gen_server:cast(?MODULE, {left, self()}).

ask() ->
  gen_server:cast(?MODULE, {ask, self()}).

stick() ->
  gen_server:cast(?MODULE, {stick, self()}).


start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

 %%% Server functions
init([]) ->
  process_flag(trap_exit, true),
  {ok, {[], dict:new()}}. %% no treatment of info here!

handle_call(terminate, _From, State) ->
   {stop, normal, ok, State}.

handle_cast({query, FromPid}, State) ->
   {RoomList, _} = State,
   FromPid ! {query, RoomList},
   {noreply, State};

handle_cast({create, FromPid}, State) ->
  {RoomList, Dict} = State,
  try dict:fetch(FromPid, Dict) of
    _ ->
      {noreply, State}
  catch
    _:_ ->
      {ok, Room} = room:start_link(?MODULE),
      {RoomList, Dict} = State,
      NewRoomList = [{Room, 1}] ++ RoomList,
      NewDict = dict:append(FromPid, Room, Dict),
      %io:format("create ~p ~p ~n", [Room, FromPid]),
      room:join(Room, FromPid),
      {noreply, {NewRoomList, NewDict}}
  end;

handle_cast({join, FromPid, Room}, State) ->
  {RoomList, Dict} = State,
  try dict:fetch(FromPid, Dict) of
    _ ->
      {noreply, State}
  catch
    _:_ ->
      {RoomList, Dict} = State,
      case lists:keyfind(Room, 1, RoomList) of
        {_, Num} ->
          NewRoomList = lists:keyreplace(Room, 1, RoomList, {Room, Num + 1}),
          NewDict = dict:append(FromPid, Room, Dict),
          room:join(Room, FromPid),
          {noreply, {NewRoomList, NewDict}};
        false -> {noreply, State}
      end
  end;

handle_cast({left, FromPid}, State) ->
  {RoomList, Dict} = State,
  try dict:fetch(FromPid, Dict) of
    [Room] ->
      {_, Num} = lists:keyfind(Room, 1, RoomList),
      NewRoomList = if
        Num > 1 -> lists:keyreplace(Room, 1, RoomList, {Room, Num - 1});
        true -> lists:keydelete(Room, 1, RoomList)
      end,
      NewDict = dict:erase(FromPid, Dict),
      room:left(Room, FromPid),
      {noreply, {NewRoomList, NewDict}}
  catch
    _:_ -> {noreply, State}
  end;

handle_cast({ask, FromPid}, State) ->
  {RoomList, Dict} = State,
  try dict:fetch(FromPid, Dict) of
    [Room] ->
      room:ask(Room, FromPid),
      {noreply, {RoomList, Dict}}
  catch
    _:_ ->
      {noreply, State}
  end;

handle_cast({stick, FromPid}, State) ->
  {RoomList, Dict} = State,
  try dict:fetch(FromPid, Dict) of
    [Room] ->
      room:stick(Room, FromPid),
      {noreply, {RoomList, Dict}}
  catch
    _:_ ->
      {noreply, State}
  end.

handle_info({'EXIT', _, normal}, State) ->
   {noreply, State};

handle_info({'EXIT', Room, _}, State) ->
   %% clean all the number
   {List, Dict} = State,
   NewList = lists:keydelete(Room, 1, List),
   NewDict = dict:filter(fun(Key, Value) ->
     not(lists:member(Room, Value))
   end, Dict),
   {noreply, {NewList, NewDict}};

handle_info(_Msg,State) ->
   io:format("Unexpected message: ~p~n",[_Msg]),
   {noreply, State}.

terminate(normal, _State) ->
   % [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
   ok;

terminate(shutdown, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
   %% No change planned. The function is there for the behaviour,
   %% but will not be used. Only a version on the next
   {ok, State}.
