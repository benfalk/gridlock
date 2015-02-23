-module(gridlock_websocket_handler).

-export([
  init/4,
  stream/3,
  info/3,
  terminate/2
]).

init(Transport, Req, Opts, Active) ->
  io:format(
    "INIT:~n--Transport: ~p~n--Req: ~p~n--Opts: ~p~n--Active:~p~n",
    [Transport, Req, Opts, Active]
  ),
  Manager = maps:get(manager, maps:from_list(Opts)),
  {ok, Req, Manager}.

stream(<<"ping">>, Req, State) ->
  {reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
  Map = jsx:decode(Data,[return_maps, {labels, atom}]),
  io:format("Stream Received:  ~p~n", [Data]),
  io:format("Data Transformed: ~p~n", [Map]),
  io:format("State: ~p~n", [State]),
  handle_event(Map, Req, State).

info(Info, Req, State) ->
  io:format("Info Received: ~p~n", [Info]),
  {ok, Req, State}.

terminate(Req, State) ->
  io:format(
    "TERMINATE:~n--Req: ~p~n--State: ~p~n",
    [Req, State]
  ),
  ok.

%%%===================================================================
%%% Internal API Handling
%%%===================================================================

handle_event(#{ event := <<"create_game">>, name := Name, size := Size}, Req, Manager) ->
  case gridlock_manager:create_game(Manager, Name, Size) of
    ok -> {ok, Req, Manager};
    {error, Reason} -> {reply, jsx:encode(#{event => <<"create_game_failed">>, reason => Reason}), Req, Manager}
  end;

handle_event(Unkown, Req, Manager) ->
  {reply, jsx:encode(#{ event => <<"invalid">>, data => Unkown}), Req, Manager}.
