-module(gridlock_grid).

-export([build/1]).

build(Size) ->
  #{size => Size, grid => #{}}.
