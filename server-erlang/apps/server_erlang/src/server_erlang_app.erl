-module(server_erlang_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

%% Application callbacks
start(_Type, _Args) ->
    server_erlang_sup:start_link().

stop(_State) ->
    ok.
