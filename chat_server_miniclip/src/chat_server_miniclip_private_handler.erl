-module(chat_server_miniclip_private_handler).

-export([handle_private_message/2]).

handle_private_message({private_message, Sender, Message, Socket}, State) ->
    io:format("LOG: Sending private message to socket: ~p from ~s: ~s~n", [Socket, Sender, Message]),
    gen_tcp:send(Socket, "Private message from " ++ Sender ++ ": " ++ Message ++ "\n"),
    {noreply, State}.