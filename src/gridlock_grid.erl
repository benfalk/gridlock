-module(gridlock_grid).

-export([
  build/1,
  square_at/2,
  surrounding_locations/2
]).

build(Size) ->
  DefaultSquare = #{has_bomb => false, status => covered, surrounding_bombs => 0},
  ProtoMap = [{{X,Y}, DefaultSquare} || X <- lists:seq(1,Size), Y <- lists:seq(1,Size)], 
  #{size => Size, grid => maps:from_list(ProtoMap)}.

square_at(#{grid := Grid}, Location) when is_map(Grid), is_tuple(Location) ->
  maps:get(Location, Grid).

surrounding_locations(#{size := Size, grid := Grid}, {X,Y}) ->
  Keys = [{X2,Y2} || X2 <- lists:seq(X-1,X+1),
                     Y2 <- lists:seq(Y-1,Y+1),
                     X2 > 0,
                     X2 =< Size,
                     Y2 > 0,
                     Y2 =< Size] -- [{X,Y}],
  Reduced = maps:with(Keys, Grid).
