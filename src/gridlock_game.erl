-module(gridlock_game).

-export([start/1,rpc/2]).

start(Size) ->
  Grid = gridlock_grid:build(Size),
  spawn(fun()-> loop(Grid) end).

rpc(Game, Method, Args) ->
  Game ! {self(), Method, Args}
  receive
    {Game, Msg} -> Msg;
    Unkown      -> io:format("Unkown msg [~p]~n", [Unkown])
  end.

rpc(Game, Method) ->
  Game ! {self(), Method}
  receive
    {Game, Msg} -> Msg;
    Unkown      -> io:format("Unkown msg [~p]~n", [Unkown])
  end.

loop(Grid) ->
  receive
    {From, uncover, {X,Y}} -> Grid2 = Grid:update({X,Y}, uncovered),
                              From ! {self(), ok},
                              loop(Grid2);

    {From, grid}           -> From ! {self(), Grid},
                              loop(Grid);

    Unkown                 -> io:format("Received unkown [~p]~n", [Unkown])
  end.
