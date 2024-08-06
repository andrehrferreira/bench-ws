-module(websocket_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

%% Supervisor callbacks
-export([init/1]).

%% Include logging macros
-include_lib("kernel/include/logger.hrl").

%% Start the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Processes = [],
    {ok, {{one_for_one, 1, 5}, Processes}}.
