-module(server_erlang_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

%% Application callbacks
start(_StartType, _StartArgs) ->
    %% Inicie o supervisor principal ou outros processos de inicialização
    io:format("Starting server_erlang application~n"),
    server_erlang_sup:start_link().

stop(_State) ->
    io:format("Stopping server_erlang application~n"),
    ok.
