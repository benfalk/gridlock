-module(gridlock_grid_tests).
-include_lib("eunit/include/eunit.hrl").

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
  Bombs = maps:fold(fun(_,#{has_bomb := B}, Ammount) -> case B of
                                                          true -> Ammount + 1;
                                                          false -> Ammount
                                                        end end, 0, Squares),
  ?assertMatch(5, Bombs).
