-module(gridlock_manager_tests).
-include_lib("eunit/include/eunit.hrl").

start_test() ->
  Manager = gridlock_manager:start(),
  ?assertMatch(X when is_pid(X), Manager).

create_grid_test() ->
  Manager = gridlock_manager:start(),
  ?assertMatch(ok, gridlock_manager:create_grid(Manager, <<"My Grid">>, 50)),
  ?assertMatch({error, already_exists}, gridlock_manager:create_grid(Manager, <<"My Grid">>, 50)).

%register_test() ->
  %Manager = gridlock_manager:start(),
  %Val = gridlock_manager:register(Manager, self()),
  %?assertMatch({ok, Ref} when is_reference(Ref), Val).
