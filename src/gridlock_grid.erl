-module(gridlock_grid).

-type gridlock_grid() :: {gridlock_grid, gridsize(), [grid(),...]}.
-type gridsize() :: integer().
-type point() :: {integer(), integer()}.
-type status() :: flagged | covered | uncovered.
-type surrounding_bombs() :: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8.
-type has_bomb() :: false | true.
-type grid() :: {point(), status(), has_bomb(), surrounding_bombs()}.

-export([build/1, update/3, surrounding_grids/2]).

build(Size) ->
  List = lists:seq(1,Size),
  Matrix = [{X, Y} || X <- List, Y <- List],
  Bombs = bomb_list(Size),
  Uncounted = {gridlock_grid, Size, merge(Matrix, Bombs, [])},
  count_squares(Uncounted).

update({X,Y}, State, {gridlock_grid, Size, Matrix}) ->
  {gridlock_grid, Size, update(Matrix, [], {X,Y}, State)}.

update([],Acc,_,_) -> lists:reverse(Acc);
update([{{X,Y},_,HasBomb,Surrounding}|Tail], Acc, {X,Y}, State) ->
  update([],lists:reverse(Tail) ++ [{{X,Y},State,HasBomb,Surrounding}|Acc], {X,Y}, State); 
update([H|T], Acc, {X,Y}, State) -> update(T, [H|Acc], {X,Y}, State).

surrounding_grids({X,Y}, {gridlock_grid, Size, _}) ->
  Over = Size + 1,
  Raw = [{X+X1,Y+Y1} || X1 <- lists:seq(-1,1), Y1 <- lists:seq(-1,1)],
  lists:delete({X,Y}, [{X2,Y2} || {X2,Y2} <- Raw, X2 > 0, Y2 > 0, X2 < Over, Y2 < Over]).

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

count_squares({gridlock_grid,Size,Matrix}) ->
  {gridlock_grid,Size,count_squares([],Matrix,Matrix)}.
count_squares(Acc,_,[]) ->
  lists:reverse(Acc);
count_squares(Acc,Matrix,[H|T]) ->
  {{X,Y},Status,HasBomb,_Surrounding} = H,
  Bombs = [Bomb || Bomb = {{X1,Y1},_,true,_} <- Matrix, X1 > X-2, X1 < X+2, Y1 > Y-2, Y1 < Y+2],
  count_squares([{{X,Y},Status,HasBomb,length(Bombs)}|Acc], Matrix, T).
