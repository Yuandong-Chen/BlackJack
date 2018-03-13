-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts, #{
        idle_timeout => 300000}}.

websocket_init(State) ->
	io:format("Pid: ~p~n", [self()]),
	{reply, {text, list_to_binary("YOUR ID IS " ++ pid_to_list(self()))}, State}.

websocket_handle({text, Msg}, State) ->
	io:format("RECEIVE ~p From ~p ~n",[Msg, self()]),
	{[{Bin, MaybeRoom}]} = jiffy:decode(Msg),
	case binary_to_atom(Bin,utf8) of
		query -> dispatcher:query();
		create -> dispatcher:create();
		join ->
		try list_to_pid(binary_to_list(MaybeRoom)) of
			Pid when is_pid(Pid) ->
				dispatcher:join(Pid)
		catch
			_:_ -> ok
		end;
		left -> dispatcher:left();
		ask -> dispatcher:ask();
		stick -> dispatcher:stick();
		_ -> ok
	end,
	{ok, State};
websocket_handle(_Data, State) ->
	io:format("UNKNOW MSG: ~p~n", [_Data]),
	{ok, State}.

% websocket_info({message, PlayerList, MakerCardList}, State) ->
% 	{reply, {text, PlayerList, MakerCardList}, State};
%
% websocket_info({message, PlayerList, MakerCardList}, State) ->
% 	{reply, {text, PlayerList, MakerCardList}, State};

websocket_info(Info, State) ->
	R = io_lib:format("~w",[Info]),
	L = lists:flatten(R),
	{reply, {text, list_to_binary(L)}, State, hibernate}.
