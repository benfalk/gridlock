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
  {ok, Req, []}.

stream(<<"ping">>, Req, State) ->
  {reply, <<"pong">>, Req, State};
stream(Data, Req, State) ->
  io:format("Stream Received:  ~p~n", [Data]),
  {ok, Req, [Data|State]}.

info(Info, Req, State) ->
  io:format("Info Received: ~p~n", [Info]),
  {ok, Req, State}.

terminate(Req, State) ->
  io:format(
    "TERMINATE:~n--Req: ~p~n--State: ~p~n",
    [Req, State]
  ),
  ok.
