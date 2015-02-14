-module(gridlock_game).

-behaviour(gen_server).

%% API functions
-export([new/1,
         get_grid/1,
         register_listener/2,
         uncover_square/2,
         flag_square/2,
         unflag_square/2
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

new(Size) ->
    {ok, GamePid} = gen_server:start_link(?MODULE, Size, []),
    GamePid.

get_grid(Game) ->
  gen_server:call(Game, get_grid).

register_listener(Game, Pid) ->
  gen_server:call(Game, {register_listener, Pid}).

uncover_square(Game, Location) ->
  gen_server:call(Game, {update_square, uncovered, Location}).

flag_square(Game, Location) ->
  gen_server:call(Game, {update_square, flagged, Location}).

unflag_square(Game, Location) ->
  gen_server:call(Game, {update_square, covered, Location}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Size) ->
    Grid = gridlock_grid:build(Size),
    Grid2 = gridlock_grid:plant_bombs(Grid, (Size*Size) div 4),
    Grid3 = gridlock_grid:count_bombs(Grid2),
    EventHandler = gen_event:start_link(),
    {ok, #{grid => Grid3, event_handler => EventHandler}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_grid, _From, State = #{grid := Grid}) ->
  {reply, gridlock_grid:squares(Grid), State};

handle_call({register_listener, Pid}, _From, State) ->
  % TODO: Need to register a listener here, gen_event deal I'm thinking
  {reply, ok, State};

handle_call({update_square, Status, Location}, _From, State = #{grid := Grid}) ->
  % TODO: Need to hook into a gen_event here?
  case gridlock_grid:update_square(Grid, Location, Status) of
    {ok, Updated}   -> {reply, ok, State#{grid := Updated}};
    {error, Reason} -> {reply, {error, Reason}, State}
  end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
