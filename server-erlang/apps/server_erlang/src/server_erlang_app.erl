%% src/server_erlang_app.erl
-module(server_erlang_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/websocket", websocket_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, 100,
        #{port => 3009},
        #{env => #{dispatch => Dispatch}}
    ),
    server_erlang_sup:start_link().

stop(_State) ->
    ok.
