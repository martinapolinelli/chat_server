-module(chat_server_miniclip_handler).

-export([start_link/0, start/1, init/1, loop/2, handle_call/3, handle_cast/2, disconnect/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, NameData} ->
            UserName = binary_to_list(NameData),
            io:format("Connection Successful for user: ~s~n", [UserName]),
            chat_server_miniclip_users:add_user(UserName, self()),
            loop(Socket, UserName);
        {error, Reason} ->
            io:format("Error receiving user data: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

disconnect(UserName, Socket) ->
    io:format("~s disconnected.~n", [UserName]),
    chat_server_miniclip_users:remove_user(UserName),
    gen_tcp:close(Socket).
    
loop(Socket, UserName) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Message = binary_to_list(Data),
            io:format("~s: ~s~n", [UserName, Message]),
            loop(Socket, UserName);
        {error, Reason} ->
            io:format("Error: ~p. Disconnecting ~s~n", [Reason, UserName]),
            disconnect(UserName, Socket)
    end.

init(Socket) ->
    io:format("Connection Successful~n"),
    {ok, #{socket => Socket, user => undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.