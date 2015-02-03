-module(gridlock_grid_tests).
-include_lib("eunit/include/eunit.hrl").

build_test() ->
  Grid = gridlock_grid:build(5),
  ?assertMatch(#{ size := 5}, Grid),
  ?assertMatch(#{ grid := G} when is_map(G), Grid).
