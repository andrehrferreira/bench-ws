-module(websocket_server_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Include logging macros
-include_lib("kernel/include/logger.hrl").

%% Application start callback
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", websocket_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, 100, [{port, 3006}], #{
        env => #{dispatch => Dispatch}
    }),
    websocket_server_sup:start_link().

%% Application stop callback
stop(_State) ->
    ok.
