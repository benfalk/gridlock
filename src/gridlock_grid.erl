-module(gridlock_grid).

-type gridlock_grid() :: {gridlock_grid, gridsize(), [grid(),...]}.
-type gridsize() :: integer().
-type point() :: {integer(), integer()}.
-type status() :: flagged | covered | uncovered.
-type surrounding_bombs() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8.
-type has_bomb() :: false | true.
-type grid() :: {point(), status(), has_bomb(), surrounding_bombs()}.

-export([build/1, fill_with/2, bomb_list/1]).

build(Size) ->
  List = lists:seq(1,Size),
  Matrix = [{X, Y} || X <- List, Y <- List],
  Bombs = bomb_list(Size),
  {gridlock_grid, Size, merge(Matrix, Bombs, [])}.

bomb_list(Size) ->
  Total = Size * Size,
  Bombs = Total div 4,
  shuffle(fill_with(true, Bombs) ++ fill_with(false, Total-Bombs)).

shuffle(List) ->
  shuffle(List, []).
shuffle([], Acc) ->
  Acc;
shuffle(List, Acc) ->
  {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
  shuffle(Leading ++ T, [H | Acc]).

fill_with(Item, Size) ->
  fill_with(Item, [], 0, Size).
fill_with(_, List, Size, Size) ->
  List;
fill_with(Item, List, Current, Size) ->
  fill_with(Item, [Item|List], Current+1, Size).

merge([],[],List) ->
  lists:reverse(List);
merge([Hm|Tm] = _Matrix, [Hb|Tb] = _Bombs, List) ->
  merge(Tm, Tb, [{Hm, covered, Hb, 0}|List]).
