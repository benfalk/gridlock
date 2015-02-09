-module(gridlock_game_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  Pid = gridlock_game:new(30),
  ?assertMatch(X when is_pid(X), Pid).

get_grid_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch(900, length(gridlock_game:get_grid(Game))),
  ?assertMatch(X when is_map(X), hd(gridlock_game:get_grid(Game))).

register_listener_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch(ok, gridlock_game:register_listener(Game, self())).

uncover_square_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch({error, invalid_location}, gridlock_game:uncover_square(Game, {31,31})),
  ?assertMatch(ok, gridlock_game:uncover_square(Game, {4,12})).
  
