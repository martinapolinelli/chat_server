-module(chat_server_miniclip_handler).

-behaviour(gen_server).

-export([start_link/1, start/1, init/1, loop/2, handle_call/3, handle_cast/2, disconnect/2, handle_info/2]).

start_link(UserName) ->
    Name = list_to_atom("chat_handler_" ++ UserName),
    {ok, Pid} = gen_server:start_link({local, Name}, ?MODULE, [], []),
    {ok, Pid}.

start(Socket) ->
    send_response(Socket, "Insert your username to start:"),
    case gen_tcp:recv(Socket, 0) of
        {ok, NameData} ->
            UserNameRaw = binary_to_list(NameData),
            UserName = string:trim(UserNameRaw),
            io:format("Connection Successful for user: ~s~n", [UserName]),
            {ok, Pid} = chat_server_miniclip_handler:start_link(UserName),
            chat_server_miniclip_users:add_user(UserName, Pid, Socket),
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
            handle_room_command(Message, UserName, Socket),
            loop(Socket, UserName);
        {error, Reason} ->
            io:format("Error: ~p. Disconnecting ~s~n", [Reason, UserName]),
            disconnect(UserName, Socket)
    end.

    handle_room_command(MessageRaw, UserName, Socket) ->
        Message = string:trim(MessageRaw),
        CleanUserName = string:trim(UserName),
        io:format("LOG: ~p. data: ~s~n", [Message, CleanUserName]),
        case string:tokens(Message, " ") of
            ["create", RoomName] ->
                CleanRoomName = string:trim(RoomName),
                case chat_server_miniclip_rooms:create_room(CleanRoomName, CleanUserName) of
                    ok ->
                        send_response(Socket, "Room created with success.");
                    {error, room_exists} ->
                        send_response(Socket, "Room already exists.");
                    _ ->
                        send_response(Socket, "Failed to create room.")
                end;
    
            ["destroy", RoomName] ->
                CleanRoomName = string:trim(RoomName),
                case chat_server_miniclip_rooms:destroy_room(CleanRoomName, CleanUserName) of
                    ok ->
                        send_response(Socket, "Room destroyed successfully.");
                    {error, room_not_found} ->
                        send_response(Socket, "Room not found.");
                    {error, not_creator} ->
                        send_response(Socket, "Action denied, only the room's creator can destroy it.");
                    _ ->
                        send_response(Socket, "Failed to destroy room.")
                end;
    
            ["list"] ->
                Rooms = chat_server_miniclip_rooms:list_rooms(),
                send_response(Socket, "Available rooms: " ++ string:join(Rooms, ", "));
    
            ["join", RoomName] ->
                CleanRoomName = string:trim(RoomName),
                case chat_server_miniclip_rooms:join_room(CleanRoomName, CleanUserName) of
                    ok ->
                        send_response(Socket, "Success! Joined room: " ++ CleanRoomName);
                    {error, room_not_found} ->
                        send_response(Socket, "Room not found.");
                    _ ->
                        send_response(Socket, "Failed to join room.")
                end;
    
            ["leave", RoomName] ->
                CleanRoomName = string:trim(RoomName),
                case chat_server_miniclip_rooms:leave_room(CleanRoomName, CleanUserName) of
                    ok ->
                        send_response(Socket, "You successfully left the room: " ++ CleanRoomName);
                    {error, room_not_found} ->
                        send_response(Socket, "Room not found.");
                    _ ->
                        send_response(Socket, "Failed to leave room.")
                end;
            
            ["send", RoomName | Rest] ->
                CleanRoomName = string:trim(RoomName),
                case Rest of
                    [] ->
                        send_response(Socket, "Error: No message provided.");
                    _ ->
                        TrimmedRest = lists:map(
                            fun(X) ->
                                case is_binary(X) of
                                    true -> binary_to_list(X);
                                    false -> string:trim(X)
                                end
                            end,
                            Rest
                        ),
                        ToSend = lists:foldl(fun(Elem, Acc) -> 
                            case Acc of
                                "" -> Elem;
                                _ -> Acc ++ " " ++ Elem
                            end
                        end, "", TrimmedRest),
                        case chat_server_miniclip_rooms:send_message(CleanRoomName, CleanUserName, ToSend) of
                            ok ->
                                send_response(Socket, "Message successfully sent to room: " ++ CleanRoomName);
                            {error, room_not_found} ->
                                send_response(Socket, "Room not found.");
                            _ ->
                                send_response(Socket, "Failed to send message.")
                        end
                end;

            %% Private message
            ["send_private", TargetUser | Tail] ->
            io:format("LOG: Private message to ~p with data ~p~n", [TargetUser, Tail]),
            case Tail of
                [] ->
                    send_response(Socket, "Error: No message provided.");
                _ ->
                    TrimmedTail = lists:map(
                        fun(X) ->
                            case is_binary(X) of
                                true -> binary_to_list(X);
                                false -> string:trim(X)
                            end
                        end,
                        Tail
                    ),
                    PrivateMessage = lists:foldl(fun(Elem, Acc) -> 
                        case Acc of
                            "" -> Elem;
                            _ -> Acc ++ " " ++ Elem
                        end
                    end, "", TrimmedTail),
                    io:format("LOG: FINAL PRIVATE MESSAGE: ~p~n", [PrivateMessage]),
                    io:format("LOG: FINAL USER: ~p~n", [CleanUserName]),
                    io:format("LOG: FINAL USER TARGET: ~p~n", [TargetUser]),
                    case chat_server_miniclip_users:send_private_message(CleanUserName, TargetUser, PrivateMessage) of
                        ok ->
                            send_response(Socket, "Private message sent to " ++ TargetUser ++ ".");
                        {error, user_not_found} ->
                            send_response(Socket, "User not found: " ++ TargetUser ++ ".");
                        _ ->
                            send_response(Socket, "Failed to send private message.")
                    end
            end;

            _ ->
                send_response(Socket, "Unknown command.")
        end.
    
send_response(Socket, Response) ->
    gen_tcp:send(Socket, Response ++ "\n").

init(Socket) ->
    io:format("Connection Successful~n"),
    {ok, #{socket => Socket, user => undefined}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({private_message, Sender, Message, Socket}, State) ->
    io:format("LOG: Received private message from ~s: ~s~n", [Sender, Message]),
    gen_tcp:send(Socket, "Private message received from " ++ Sender ++ ": " ++ Message ++ "\n"),
    {noreply, State};
handle_info(Unknown, State) ->
    io:format("LOG: Unknown message received in handle_info: ~p~n", [Unknown]),
    {noreply, State}.