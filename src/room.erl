-module (room).
-behaviour (gen_fsm).
-export ([start/1, start_link/1]).
-export ([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export ([join/2, left/2, ask/2, stick/2]).
-export ([sumcards/1, give2maker/2, calculate/1, broadcast/3]).
-export ([stage1/2, givecards/2, all_call/2, call/2]).
%% super naive state, store Pid of Players
-record (state, {players=[], buffers=[], waitings=[], cardsleft=[], register_server, makercard=[]}).
-record (player, {id, cardshold=[], credit = 0}).
-record (message, {roomid, players=[], makercard=[]}).

all_call(Pid, E) ->
  gen_fsm:send_all_state_event(Pid, E).

call(Pid, E) ->
  gen_fsm:send_event(Pid, E).

%APIs

ready(Pid) ->
  room:call(Pid, ready).

join(Pid, Name) ->
  all_call(Pid, {join, Name}),
  ready(Pid),
  gen_fsm:send_all_state_event(Pid, broadcast).

left(Pid, Name) ->
  all_call(Pid, {left, Name}),
  gen_fsm:send_all_state_event(Pid, broadcast).

ask(Pid, Name) ->
  room:call(Pid, {ask, Name}).

stick(Pid, Name) ->
  room:call(Pid, {stick, Name}).

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).
start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

deck(N) when N > 0 ->
  lists:seq(1,13) ++ deck(N - 1);
deck(0) ->
  [].

sumcards([H|T]) when is_integer(H) ->
  I = case H of
    1 -> {1, 1};
    X when X > 10 ->
      {0, 10};
    X -> {0, X}
  end,
  {PlusA, Least} = I,
  {A, LeastPoint} = sumcards(T),
  {PlusA+A, Least+LeastPoint};

sumcards([]) ->
  {0, 0}.

is_bust(S = #player{}) when is_record(S, player) ->
  {_, P} = sumcards(S#player.cardshold), P > 21.

get_point(S = #player{}) when is_record(S, player) ->
  {A, P} = sumcards(S#player.cardshold),
  room:calculate({A, P}).

calculate({A, P}) ->
  if
    A > 1, P + 20 =< 21 ->
      P + 20;
    A > 0, P + 10 =< 21 ->
      P + 10;
    true ->
      P
  end.

give2cards([P|T], [C1,C2|C], Collector) ->
  P2 = P#player{cardshold = [C1, C2]},
  Collector2 = [P2|Collector],
  give2cards(T, C, Collector2);

give2cards([], L , Collector) ->
  {L, lists:reverse(Collector)}.

give2maker(Makers, [H|Left]) ->
  {A, P} = sumcards(Makers),
  Point  = room:calculate({A, P}),
  if
    Point > 16 -> {Makers, [H|Left]};
    true -> give2maker([H]++Makers, Left)
  end.


checking(Pointofmaker, [H | T], Collector) when is_record(H, player)->
  Pointofplayer = get_point(H),
  Bool = is_bust(H),
  if
    Pointofmaker > 21 orelse Pointofmaker < Pointofplayer ->
      if
        Bool == false ->
          Credit = H#player.credit,
          R = H#player{credit = Credit + 100, cardshold = []},
          checking(Pointofmaker, T, [R|Collector]);
        true ->
          Credit = H#player.credit,
          R = H#player{credit = Credit - 100, cardshold = []},
          checking(Pointofmaker, T, [R|Collector])
      end;
    Pointofmaker > Pointofplayer ->
      Credit = H#player.credit,
      R = H#player{credit = Credit - 100, cardshold = []},
      checking(Pointofmaker, T, [R|Collector]);
    Pointofmaker == Pointofplayer ->
      Credit = H#player.credit,
      R = H#player{credit = Credit, cardshold = []},
      checking(Pointofmaker, T, [R|Collector])
  end;

checking(_Pointofmaker, [], Collector) ->
  Collector.

broadcast([H|T], S = #message{}, reveal) ->
  H#player.id ! S,
  broadcast(T, S, reveal);

broadcast([H|T], S = #message{}, normal) ->
  [H1, _H2] = S#message.makercard,
  H#player.id ! S#message{makercard = [H1, 0]},
  broadcast(T, S, normal);

broadcast([], _, _) -> ok.

init([Name]) ->
  State = #state{register_server = Name},
  {ok, stage1, State}.

stage1(_, S = #state{players = [], waitings = []}) ->
  % io:format("room is empty: ~p~n", [S]),
  {next_state, stage1, S};

stage1(_, S = #state{}) ->
  Leftlength = length(S#state.cardsleft),
  Cards = if
    Leftlength < 52 ->
      shuffle:list(deck(8));
    true ->
      S#state.cardsleft
  end,
  P = S#state.players, P = [], B = S#state.buffers, B = [],%% assert
  %% 2 cards to every players...
  {[H1,H2|Cardsleft],Players} = give2cards(S#state.waitings, Cards, []),
  {next_state, givecards, S#state{cardsleft=Cardsleft, players = Players, waitings = [], makercard = [H1,H2]}}.

givecards(_, S = #state{players = []}) ->
  % io:format("checking: ~p ~n", [S]),
  % give cards to maker
  {Makercard, Cardsleft} = give2maker(S#state.makercard, S#state.cardsleft),
  Pointofmaker = room:calculate(sumcards(Makercard)),
  Players = checking(Pointofmaker, S#state.buffers, []) ++ S#state.waitings,
  gen_fsm:send_all_state_event(self(), reveal),
  ready(self()),
  gen_fsm:send_all_state_event(self(), broadcast),
  {next_state, stage1, S#state{players =[], buffers = [], makercard = Makercard, waitings = Players, cardsleft = Cardsleft} };

givecards({stick, ID}, S = #state{}) ->
  case lists:keyfind(ID, 2, S#state.players) of
    false -> {next_state, givecards, S};
    Player ->
       room:call(self(), check),
       Players = lists:keydelete(ID, 2, S#state.players),
       Buffers = [Player] ++ S#state.buffers,
       {next_state, givecards, S#state{players = Players, buffers = Buffers } }
  end;

givecards({ask, ID}, S = #state{}) ->
  case lists:keyfind(ID, 2, S#state.players) of
    false -> {next_state, givecards, S};
    Player ->
    case is_bust(Player) of
      true -> {next_state, givecards, S};
      false ->
        [H | T] = S#state.cardsleft,
        Cardshold = [H] ++ Player#player.cardshold,
        NewPlayer = Player#player{cardshold = Cardshold},
        gen_fsm:send_all_state_event(self(), broadcast),
        case is_bust(NewPlayer) of
          true ->
           room:call(self(), check),
           Players = lists:keydelete(ID, 2, S#state.players),
           Buffers = [NewPlayer] ++ S#state.buffers,
           {next_state, givecards, S#state{players = Players, buffers = Buffers, cardsleft = T } };
          false ->
           Players = lists:keyreplace(ID, 2, S#state.players, NewPlayer),
           {next_state, givecards, S#state{players = Players, cardsleft = T} }
        end
    end
  end;

givecards(_, S = #state{}) ->
  % io:format("stalling: ~p ~n", [S]),
  {next_state, givecards, S}.

handle_event({join, ID}, StateName, S = #state{}) ->
    Players = S#state.waitings,
    R = S#state{waitings = [#player{id = ID}|Players]},
    % io:format("~p joins and is waiting for the game~n", [ID]),
    monitor(process, ID),
    {next_state, StateName, R};

handle_event({left, ID}, StateName, S = #state{}) ->
    %% find the first vac position
    Players = S#state.players,
    Buffers = S#state.buffers,
    Waiters = S#state.waitings,
    PlayersF = lists:keydelete(ID, 2, Players),
    BuffersF = lists:keydelete(ID, 2,  Buffers),
    WaitersF = lists:keydelete(ID, 2, Waiters),
    % io:format("~p left room~n", [ID]),
    Sum = length(PlayersF) + length(BuffersF) + length(WaitersF),
    if
      Sum == 0 ->
        {stop, normal, S};
      true ->
        room:call(self(), check),
        {next_state, StateName, S#state{players = PlayersF, buffers = BuffersF, waitings = WaitersF} }
    end;

handle_event(reveal, StateName, S = #state{}) ->
    % io:format("broadcast info: ~p~n", [S]),
    Msg = #message{roomid=self(), players = S#state.players ++ S#state.buffers ++ S#state.waitings, makercard = S#state.makercard},
    broadcast(S#state.players ++ S#state.buffers ++ S#state.waitings, Msg, reveal),
    {next_state, StateName, S};

handle_event(broadcast, StateName, S = #state{}) ->
    % io:format("broadcast info: ~p~n", [S]),
    Msg = #message{roomid=self(), players = S#state.players ++ S#state.buffers ++ S#state.waitings, makercard = S#state.makercard},
    broadcast(S#state.players ++ S#state.buffers ++ S#state.waitings, Msg, normal),
    {next_state, StateName, S};

handle_event(_Event, StateName, Data) ->
    % io:format("ignoring: ~p~n", [Data]),
    {next_state, StateName, Data}.

handle_sync_event(_Event, _From, StateName, Data) ->
    {next_state, StateName, Data}.

handle_info({'DOWN', _, _, ID, _}, StateName, Data) ->
    gen_server:cast(dispatcher, {left, ID}),
    {next_state, StateName, Data};

handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
 {ok, StateName, Data}.

%% Transaction completed.
terminate(_Reason, _StateName, _StateData) ->
    ok.
