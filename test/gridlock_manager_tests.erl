-module(gridlock_manager_tests).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
  Manager = gridlock_manager:start(),
  ?assertMatch(X when is_pid(X), Manager).

create_game_test() ->
  Manager = gridlock_manager:start(),
  ?assertMatch(ok, gridlock_manager:create_game(Manager, <<"My Grid">>, 10)),
  ?assertMatch({error, already_exists}, gridlock_manager:create_game(Manager, <<"My Grid">>, 10)).

game_list_test() ->
  Manager = gridlock_manager:start(),
  ok = gridlock_manager:create_game(Manager, <<"My Grid">>, 10),
  ?assertMatch([<<"My Grid">>], gridlock_manager:game_list(Manager)).

with_game_test() ->
  Manager = gridlock_manager:start(),
  gridlock_manager:create_game(Manager, <<"My Grid">>, 10),
  ?assertMatch(G when is_list(G), gridlock_manager:with_game(Manager, <<"My Grid">>, get_grid)).

register_test() ->
  Manager = gridlock_manager:start(),
  Val = gridlock_manager:register(Manager, self()),
  ?assertMatch({ok, Ref} when is_reference(Ref), Val),

  ok = gridlock_manager:create_game(Manager, <<"My Grid">>, 10),
  Msg = receive
    Any -> Any
  after 10 ->
    nope
  end,
  Expected = #{event => game_created,name => <<"My Grid">>,size => 10},
  ?assertMatch(Expected, Msg).
