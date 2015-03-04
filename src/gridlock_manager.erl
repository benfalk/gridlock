-module(gridlock_manager).

-behaviour(gen_server).

%% API functions
-export([
  start/0
  ,create_game/3
  ,game_list/1
  ,register/2
  ,with_game/3
  ,with_game/4
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

start() ->
  {ok, ManagerPid} = gen_server:start_link(?MODULE, [], []),
  ManagerPid.

create_game(Manager, Name, Size) when is_pid(Manager), is_binary(Name), is_integer(Size) ->
  gen_server:call(Manager, {create_game, #{name => Name, size => Size}}).

game_list(Manager) ->
  gen_server:call(Manager, game_list).

with_game(Manager, Game, Command) ->
  gen_server:call(Manager, {with_game, Game, Command, []}).
with_game(Manager, Game, Command, Args) ->
  gen_server:call(Manager, {with_game, Game, Command, Args}).

register(Manager, Pid) ->
  gen_server:call(Manager, {register, Pid}).
  

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
init(_State) ->
  {ok, EventListener} = gen_event:start_link(),
  {ok, #{grids => #{}, event_listener => EventListener}}.

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
handle_call({register, Pid}, _From, State = #{ event_listener := EventListener }) ->
  Val = gridlock_listener:add_listener(EventListener, Pid),
  {reply, Val, State};

handle_call({create_game, GameData = #{name := Name, size := Size}}, _From, State = #{ grids := Grids }) ->
  case maps:is_key(Name, Grids) of
    true -> {reply, {error, already_exists}, State};
    false -> AddedGrid = maps:put(Name, gridlock_game:new(Size), Grids),
             notify_event(State, game_created, GameData),
             {reply, ok, State#{ grids := AddedGrid }}
  end;

handle_call({with_game, Game, Command, Args}, _From, State = #{ grids := Grids }) ->
  Grid = maps:get(Game, Grids),
  Reply = erlang:apply(gridlock_game, Command, [Grid]++Args),
  global_event(Command, Game, Args, Reply, State),
  {reply, Reply, State};

handle_call(game_list, _From, State = #{ grids := Grids }) ->
  {reply, maps:keys(Grids), State};

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
global_event(uncover_square, Game, [{X,Y}], ok, State) ->
  notify_event(State, uncover_square, #{ name => Game, location => #{ x => X, y => Y}});

global_event(flag_square, Game, [{X,Y}], ok, State) ->
  notify_event(State, flag_square, #{ name => Game, location => #{ x => X, y => Y}});

global_event(unflag_square, Game, [{X,Y}], ok, State) ->
  notify_event(State, unflag_square, #{ name => Game, location => #{ x => X, y => Y}});

global_event(_Command, _Game, _Args, _Reply, _State) -> nil.

notify_event(State, EventName, Data) when is_map(Data), is_atom(EventName) ->
  notify(State, maps:put(event, EventName, Data)).

notify(#{ event_listener := EventListener}, Msg) ->
  gen_event:notify(EventListener, Msg).
