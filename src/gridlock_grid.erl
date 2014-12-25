-module(gridlock_grid).

-type gridlock_grid() :: {gridlock_grid,[grid(),...]}.
-type point() :: {integer(), integer()}.
-type status() :: flagged | covered | uncovered.
-type surrounding_bombs() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8.
-type has_bomb() :: false | true.
-type grid() :: {point(), status(), has_bomb(), surrounding_bombs()}.

-export([build/1]).

build(Size) ->
  List = lists:seq(1,Size),
  Matrix = [{{X, Y}, covered, false, 0} || X <- List, Y <- List],
  {gridlock_grid, Matrix}.

%bomb_list(Size) ->
  %Total = Size * Size,
  %Bombs = Total div 4,
  %shuffle(bombs(Bombs) ++ no_bombs(Total-Bombs)).

shuffle(List) ->
  shuffle(List, []).
shuffle([], Acc) ->
  Acc;
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).
