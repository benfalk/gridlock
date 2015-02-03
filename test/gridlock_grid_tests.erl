-module(gridlock_grid_tests).
-include_lib("eunit/include/eunit.hrl").

build_test() ->
  Grid = gridlock_grid:build(5),
  ?assertMatch(#{ size := 5}, Grid),
  ?assertMatch(#{ grid := G} when is_map(G), Grid).

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
  ?assertMatch(#{{5,4} := _}, Locations5x5).
