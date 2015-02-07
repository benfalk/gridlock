-module(gridlock_grid).

-export([
  build/1,
  square_at/2,
  surrounding_locations/2,
  plant_bombs/2,
  count_bombs/1,
  update_square/3,
  squares/1
]).

build(Size) ->
  DefaultSquare = #{has_bomb => false, status => covered, surrounding_bombs => 0, location => {0,0}},
  ProtoMap = [{{X,Y}, DefaultSquare#{location := {X,Y}}} || X <- lists:seq(1,Size), Y <- lists:seq(1,Size)], 
  #{size => Size, squares => maps:from_list(ProtoMap)}.

square_at(#{squares := Grid}, Location) when is_map(Grid), is_tuple(Location) ->
  maps:get(Location, Grid).

surrounding_locations(#{size := Size, squares := Grid}, {X,Y}) ->
  Keys = [{X2,Y2} || X2 <- lists:seq(X-1,X+1),
                     Y2 <- lists:seq(Y-1,Y+1),
                     X2 > 0,
                     X2 =< Size,
                     Y2 > 0,
                     Y2 =< Size] -- [{X,Y}],
  maps:with(Keys, Grid).

plant_bombs(Grid = #{size := Size, squares := Squares}, BombAmount) ->
  BombList = lists:duplicate(BombAmount, true) ++ lists:duplicate(Size*Size-BombAmount, false),
  SquaresList = maps:to_list(Squares),
  Seeded = plant_bombs(shuffle(BombList), SquaresList, []),
  Grid#{squares := maps:from_list(Seeded)}.

plant_bombs([],[],List) -> lists:reverse(List);
plant_bombs([Bomb|BombList], [{Cord,Square}|SquaresList], Acc) ->
  plant_bombs(BombList, SquaresList, [{Cord,Square#{has_bomb := Bomb}}|Acc]).

count_bombs(Grid = #{ squares := Squares }) ->
  CountBombs = fun(L, S, A) ->
    maps:put(L, S#{ surrounding_bombs := tally_bombs(Grid, L)}, A)
  end,
  Grid#{squares := maps:fold(CountBombs, Squares, Squares)}.

update_square(Grid = #{ squares := Squares }, Location, Status) ->
  Square = maps:put(status, Status, square_at(Grid, Location)),
  { ok, Grid#{ squares := maps:put(Location, Square, Squares) } }.

tally_bombs(Grid, L) ->
  maps:fold(fun(_,#{has_bomb := true}, Amount) -> Amount + 1;
               (_,#{has_bomb := false}, Amount) -> Amount
            end, 0, surrounding_locations(Grid, L)).

squares(#{squares := Squares}) ->
  maps:values(Squares).

% Shuffle a list to randomize the order of the elements
shuffle(L) ->
  shuffle(list_to_tuple(L), length(L)).
shuffle(T, 0)->
  tuple_to_list(T);
shuffle(T, Len)->
  Rand = random:uniform(Len),
  A = element(Len, T),
  B = element(Rand, T),
  T1 = setelement(Len, T,  B),
  T2 = setelement(Rand,  T1, A),
  shuffle(T2, Len - 1).
