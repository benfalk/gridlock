-module(gridlock_game).

-export([start/1]).

start(Size) ->
  Grid = gridlock_grid:build(Size),
  spawn(fun()-> loop(Grid) end).

loop(Grid) ->
  receive
    {From, uncover, {X,Y}} -> Grid2 = Grid:update({X,Y}, uncovered),
                              From ! {self(), ok},
                              loop(Grid2);

    Unkown                 -> io:format("Received unkown [~p]~n", [Unkown])
  end.
