-module(gridlock_grid).

%-spec gridlock_grid() :: {gridlock_grid,[grid(),...]}.
%-spec point() :: {integer(), integer()}.
%-spec status() :: flagged | covered | uncovered.
%-spec surrounding_bombs() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8.
%-spec has_bomb() :: false | true.
%-spec grid() :: {point(), status(), has_bomb(), surrounding_bombs()}.

export([build/1, list_upto/1]).

build(Size) ->
  List = list_upto(Size),
  {gridlock_grid, [{X, Y}, covered, false, 0} || X <- List, Y <- List]}.


list_upto(Size) ->
  list_upto(0, Size, []).

list_upto(Size, Size, List) ->
  lists:reverse([Size|List]);
list_upto(Current, Max, List) ->
  list_upto(Current + 1, Max, [Current + 1 | List]).
