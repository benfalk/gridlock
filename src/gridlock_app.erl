-module(gridlock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("Starting Crypto....~n"),
    ok = application:start(crypto),

    io:format("Starting Ranch...~n"),
    ok = application:start(ranch),

    io:format("Starting Cowlib...~n"),
    ok = application:start(cowlib),

    io:format("Starting Cowboy...~n"),
    ok = application:start(cowboy),

    io:format("Building Route Table...~n"),
    Dispatch = cowboy_router:compile([
      {'_', [
        {"/", cowboy_static, {file, "assets/index.html"}},
        {"/css/[...]", cowboy_static, {dir, "assets/css/"}},
        {"/js/bullet.js", cowboy_static, {priv_file, bullet, "bullet.js"}},
        {"/js/[...]", cowboy_static, {dir, "assets/js"}},
        {"/fonts/[...]", cowboy_static, {dir, "assets/fonts"}},
        {"/gridlock", bullet_handler, [
          {handler, gridlock_websocket_handler},
          {manager, gridlock_manager:start()}
        ]}
      ]}
    ]),

    io:format("Starting HTTP...~n"),
    cowboy:start_http(
      my_gridlock_web_server,
      10, % Number of "workers"
      [{port, 8080}],
      [{env, [{dispatch, Dispatch}]}]
    ),

    io:format("Starting Supervisor...~n"),
    gridlock_sup:start_link().

stop(_State) ->
    ok.
