-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% Função init/2
init(Req, _Opts) ->
    {cowboy_websocket, Req, #{}}.

%% Inicialização do WebSocket
websocket_init(State) ->
    {ok, State}.

%% Tratamento de mensagens recebidas
websocket_handle({text, Msg}, State) ->
    {reply, {text, <<"Echo: ", Msg/binary>>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

%% Tratamento de outras informações
websocket_info(_Info, State) ->
    {ok, State}.

%% Função de término
terminate(_Reason, _Req, _State) ->
    ok.
