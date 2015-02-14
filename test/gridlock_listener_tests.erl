-module(gridlock_listener_tests).
-include_lib("eunit/include/eunit.hrl").

add_as_handler_test() ->
  {ok, EH} = gen_event:start_link(),
  Ref = {gridlock_listener, make_ref()},
  ok = gen_event:add_handler(EH, Ref, [self()]),
  ok.

handle_square_change_test() ->
  {ok, EH} = gen_event:start_link(),
  Ref = {gridlock_listener, make_ref()},
  ok = gen_event:add_handler(EH, Ref, [self()]),
  ok = gen_event:notify(EH, {square_changed, roflcopter}),
  Msg = receive
    Any -> Any
  after 10 ->
    nope
  end,
  ?assertMatch({square_changed, roflcopter}, Msg).

add_listener_test() ->
  {ok, EH} = gen_event:start_link(),
  ?assertMatch({ok, Ref} when is_reference(Ref), gridlock_listener:add_listener(EH, self())).
