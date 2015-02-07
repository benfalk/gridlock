-module(gridlock_game_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
  Pid = gridlock_game:new(30),
  ?assertMatch(X when is_pid(X), Pid).
