-module(gridlock_grid_tests).
-include_lib("eunit/include/eunit.hrl").

grid_structure() ->
  #{size => 3,
    squares => #{
      {1,1} => square_structure(),     
      {1,2} => square_structure(),     
      {1,3} => square_structure(),      
      {2,1} => square_structure(),      
      {2,2} => square_structure(),      
      {2,3} => square_structure(),      
      {3,1} => square_structure(),      
      {3,2} => square_structure(),      
      {3,3} => square_structure()       
    }
  }.
square_structure() ->
  #{has_bomb => false, status => covered, surrounding_bombs => 0}.

build_test() ->
  Grid = gridlock_grid:build(5),
  ?assertMatch(#{ size := 5}, Grid),
  ?assertMatch(#{ squares := G} when is_map(G), Grid).

square_at_test() ->
  Grid = gridlock_grid:build(5),
  Square = gridlock_grid:square_at(Grid, {1,1}),
  ?assertMatch(#{ has_bomb := _, status :=_, surrounding_bombs :=_}, Square).

surrounding_locations_test() ->
  Grid = gridlock_grid:build(5),
  Locations1x1 = gridlock_grid:surrounding_locations(Grid, {1,1}),
  ?assertMatch(X when is_map(X), Locations1x1),
  ?assertMatch(3, maps:size(Locations1x1)),
  ?assertMatch(#{{1,2} := _}, Locations1x1),
  ?assertMatch(#{{2,1} := _}, Locations1x1),
  ?assertMatch(#{{2,2} := _}, Locations1x1),

  Locations5x5 = gridlock_grid:surrounding_locations(Grid, {5,5}),
  ?assertMatch(X when is_map(X), Locations5x5),
  ?assertMatch(3, maps:size(Locations5x5)),
  ?assertMatch(#{{4,5} := _}, Locations5x5),
  ?assertMatch(#{{4,4} := _}, Locations5x5),
  ?assertMatch(#{{5,4} := _}, Locations5x5),

  Locations2x2 = gridlock_grid:surrounding_locations(Grid, {2,2}),
  ?assertMatch(X when is_map(X), Locations2x2),
  ?assertMatch(8, maps:size(Locations2x2)),
  ?assertMatch(#{{1,1} := _}, Locations2x2),
  ?assertMatch(#{{1,2} := _}, Locations2x2),
  ?assertMatch(#{{1,3} := _}, Locations2x2),
  ?assertMatch(#{{2,1} := _}, Locations2x2),
  ?assertMatch(#{{2,3} := _}, Locations2x2),
  ?assertMatch(#{{3,1} := _}, Locations2x2),
  ?assertMatch(#{{3,2} := _}, Locations2x2),
  ?assertMatch(#{{3,3} := _}, Locations2x2).

plant_bombs_test() ->
  Grid = gridlock_grid:build(5),
  #{size:= 5, squares := Squares} = gridlock_grid:plant_bombs(Grid, 5),

  % Count up all the bombs
  Bombs = maps:fold(fun(_,#{has_bomb := B}, Ammount) -> case B of
                                                          true -> Ammount + 1;
                                                          false -> Ammount
                                                        end end, 0, Squares),
  ?assertMatch(5, Bombs),
  ?assertMatch(#{{5,5} := _ }, Squares),
  ?assertMatch(#{{1,1} := _ }, Squares).

count_bombs_test() ->
  Grid = #{ squares := Squares } = grid_structure(),
  Seeded = Grid#{ squares := maps:put({1,2}, maps:put(has_bomb, true, square_structure()), Squares)},
  #{ squares := SeededSquares } = gridlock_grid:count_bombs(Seeded),
  WithAmount = maps:fold(fun(_, #{surrounding_bombs := 0}, Ammount) -> Ammount;
                            (_, #{surrounding_bombs := _}, Ammount) -> Ammount +1
                         end, 0, SeededSquares),
  ?assertMatch(X when X > 0, WithAmount),
  
  CountedSquare = maps:get({1,1}, SeededSquares),
  ?assertMatch(#{surrounding_bombs := 1}, CountedSquare).

