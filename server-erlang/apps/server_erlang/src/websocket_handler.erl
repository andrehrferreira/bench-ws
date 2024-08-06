-module(websocket_handler).
-behaviour(cowboy_websocket).

%% API
-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/3]).

%% Include logging macros
-include_lib("kernel/include/logger.hrl").

%% Initialization
init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~s~n", [Msg]),
    %% Echo the message back to the client
    {reply, {text, Msg}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
