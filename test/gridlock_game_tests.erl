-module(gridlock_game_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  Pid = gridlock_game:new(30),
  ?assertMatch(X when is_pid(X), Pid).

get_grid_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch(900, length(gridlock_game:get_grid(Game))),
  ?assertMatch(X when is_map(X), hd(gridlock_game:get_grid(Game))).
