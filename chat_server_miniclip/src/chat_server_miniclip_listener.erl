-module(chat_server_miniclip_listener).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PORT, 8080).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    spawn(fun() -> accept_connections(ListenSocket) end),
    {ok, ListenSocket}.

accept_connections(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> chat_server_miniclip_handler:start(Socket) end),
            accept_connections(ListenSocket);
        {error, Reason} ->
            io:format("Connection was not accepted: ~p~n", [Reason])
    end.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.