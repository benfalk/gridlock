-module(gridlock_game).

-export([start/1,rpc/2,rpc/3]).

start(Size) ->
  Grid = gridlock_grid:build(Size),
  Callbacks = [],
  spawn(fun()-> loop(Grid, Callbacks) end).

rpc(Game, Method, Args) ->
  Game ! {self(), Method, Args},
  receive
    {Game, Msg} -> Msg;
    Unkown      -> io:format("Unkown msg [~p]~n", [Unkown])
  end.

rpc(Game, Method) ->
  Game ! {self(), Method},
  receive
    {Game, Msg} -> Msg;
    Unkown      -> io:format("Unkown msg [~p]~n", [Unkown])
  end.

loop(Grid, Callbacks) ->
  receive
    {From, uncover, {X,Y}} -> Grid2 = Grid:update({X,Y}, uncovered),
                              From ! {self(), ok},
                              [Fun({X,Y}) || Fun <- Callbacks],
                              loop(Grid2, Callbacks);

    {From, grid}           -> From ! {self(), Grid},
                              loop(Grid, Callbacks);

    {From, callback, Fun}  -> Callbacks2 = [Fun|Callbacks],
                              From ! {self(), ok},
                              loop(Grid, Callbacks2);

    Unkown                 -> io:format("Received unkown [~p]~n", [Unkown])
  end.
