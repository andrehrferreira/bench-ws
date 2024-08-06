%% src/server_erlang_sup.erl
-module(server_erlang_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 10, 10}, [
        {websocket_handler, {websocket_handler, start_link, []}, permanent, 1000, worker, [websocket_handler]}
    ]}}.
