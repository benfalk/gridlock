-module(gridlock_game_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  Pid = gridlock_game:new(30),
  ?assertMatch(X when is_pid(X), Pid).

size_test() ->
  Game = gridlock_game:new(5),
  ?assertMatch(5, gridlock_game:size(Game)).

get_grid_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch(900, length(gridlock_game:get_grid(Game))),
  ?assertMatch(X when is_map(X), hd(gridlock_game:get_grid(Game))).

register_listener_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch(ok, gridlock_game:register_listener(Game, self())),
  gridlock_game:flag_square(Game, {1,1}),
  Msg =
    receive
      Any -> Any
    after 10 ->
      nope
    end,
  ?assertMatch({square_changed, #{ status := flagged, location := _Loc }}, Msg).

uncover_square_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch({error, invalid_location}, gridlock_game:uncover_square(Game, {31,31})),
  ?assertMatch(ok, gridlock_game:uncover_square(Game, {4,12})).
  
flag_square_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch({error, invalid_location}, gridlock_game:flag_square(Game, {31,31})),
  ok = gridlock_game:uncover_square(Game, {1,1}),
  ?assertMatch({error, cannot_change_uncovered}, gridlock_game:flag_square(Game, {1,1})),
  ?assertMatch(ok, gridlock_game:flag_square(Game, {4,12})).

unflag_square_test() ->
  Game = gridlock_game:new(30),
  ?assertMatch({error, invalid_location}, gridlock_game:unflag_square(Game, {31,31})),
  ok = gridlock_game:uncover_square(Game, {1,1}),
  ?assertMatch({error, cannot_change_uncovered}, gridlock_game:unflag_square(Game, {1,1})),
  ?assertMatch(ok, gridlock_game:unflag_square(Game, {4,12})).
