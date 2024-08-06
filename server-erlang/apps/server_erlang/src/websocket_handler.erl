%% src/websocket_handler.erl
-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/3]).

-record(state, {clients = #{}, client_count = 0}).

%% Iniciar o WebSocket
init(Req, State) ->
    {cowboy_websocket, Req, State}.

%% Inicialização do WebSocket
websocket_init(State) ->
    {ok, State}.

%% Manipulador de mensagens recebidas
websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~s~n", [Msg]),
    Message = <<"Message from server: ", Msg/binary>>,
    %% Iterar sobre todos os clientes e enviar a mensagem
    maps:fold(fun(_, WS, Acc) ->
        WS ! {text, Message},
        Acc
    end, ok, State#state.clients),
    {ok, State}.

%% Manipulador de mensagens de informação
websocket_info(_Info, State) ->
    {ok, State}.

%% Terminador do WebSocket
websocket_terminate(_Reason, _Req, State) ->
    io:format("Connection closed~n", []),
    {ok, State}.
