-module(chat_server_miniclip_handler).

-behaviour(gen_server).

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
                    io:format("LOG2: RoomName=~p, Rest=~p~n", [RoomName, Rest]),
                    CleanRoomName = string:trim(RoomName),
                    case Rest of
                        [] ->
                            send_response(Socket, "Error: No message provided.");
                        _ ->
                            Message = string:join(lists:map(fun string:trim/1, Rest), " "),
                            case chat_server_miniclip_rooms:send_message(CleanRoomName, CleanUserName, Message) of
                                ok ->
                                    send_response(Socket, "Message successfully sent to room: " ++ CleanRoomName);
                                {error, room_not_found} ->
                                    send_response(Socket, "Room not found.");
                                _ ->
                                    send_response(Socket, "Failed to send message.")
                            end
                    end;
    
            ["send"] ->
                send_response(Socket, "Invalid command. Use: send RoomName Message");
    
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