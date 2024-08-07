-module(server_erlang_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", websocket_handler, []}
        ]}
    ]),
    {ok, {{one_for_one, 10, 10},
        [
            {cowboy_listener, {cowboy, start_clear, [http_listener, 100,
                #{port => 3009},
                #{env => #{dispatch => Dispatch}}]},
            permanent, 1000, worker, [cowboy]}
        ]}}.
