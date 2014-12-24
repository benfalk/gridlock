-module(gridlock_grid).

-type gridlock_grid() :: {gridlock_grid,[grid(),...]}.
-type point() :: {integer(), integer()}.
-type status() :: flagged | covered | uncovered.
-type surrounding_bombs() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8.
-type has_bomb() :: false | true.
-type grid() :: {point(), status(), has_bomb(), surrounding_bombs()}.

-export([build/1, list_upto/1]).

build(Size) ->
  List = list_upto(Size),
  Matrix = [{{X, Y}, covered, false, 0} || X <- List, Y <- List],
  {gridlock_grid, Matrix}.

list_upto(Size) ->
  list_upto(0, Size, []).

list_upto(Size, Size, List) ->
  lists:reverse(List);
list_upto(Current, Max, List) ->
  list_upto(Current + 1, Max, [Current + 1 | List]).
