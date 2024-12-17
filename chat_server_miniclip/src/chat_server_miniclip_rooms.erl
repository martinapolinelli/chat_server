-module(chat_server_miniclip_rooms).

-behaviour(gen_server).

-export([start_link/0, create_room/3, invite_user/3, destroy_room/2, list_rooms/1, join_room/2, leave_room/2, send_message/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{rooms => #{}}}.

create_room(RoomName, Creator, IsPrivate) ->
    gen_server:call(?MODULE, {create_room, RoomName, Creator, IsPrivate}).

invite_user(RoomName, Host, Guest) ->
    gen_server:call(?MODULE, {invite_user, RoomName, Host, Guest}).

destroy_room(RoomName, UserName) ->
    gen_server:call(?MODULE, {destroy_room, RoomName, UserName}).

%% Filtered
list_rooms(UserName) ->
    gen_server:call(?MODULE, {list_rooms, UserName}).

join_room(RoomName, User) ->
    gen_server:call(?MODULE, {join_room, RoomName, User}).

leave_room(RoomName, User) ->
    gen_server:call(?MODULE, {leave_room, RoomName, User}).

send_message(RoomName, Sender, Message) ->
    gen_server:call(?MODULE, {send_message, RoomName, Sender, Message}).

handle_call({create_room, RoomName, Creator, IsPrivate}, _From, State) ->
    Rooms = maps:get(rooms, State, #{}),
    case maps:is_key(RoomName, Rooms) of
        true ->
            {reply, {error, room_exists}, State};
        false ->
            NewRoom = #{creator => Creator, users => [Creator], is_private => IsPrivate, invited_users => [Creator]},
            UpdatedRooms = maps:put(RoomName, NewRoom, Rooms),
            {reply, ok, State#{rooms => UpdatedRooms}}
    end;

handle_call({invite_user, RoomName, Host, Guest}, _From, State) ->
    Rooms = maps:get(rooms, State, #{}),
    case maps:get(RoomName, Rooms, undefined) of
        undefined ->
            {reply, {error, room_not_found}, State};
        Room ->
            Creator = maps:get(creator, Room),
            Guests = maps:get(invited_users, Room, []),
            case Host of
                Creator ->
                    UpdatedRoom = Room#{invited_users => [Guest | Guests]},
                    UpdatedRooms = maps:put(RoomName, UpdatedRoom, Rooms),
                    {reply, ok, State#{rooms => UpdatedRooms}};
                _ ->
                    {reply, {error, not_creator}, State}
            end
    end;

    handle_call({destroy_room, RoomNameRaw, UserNameRaw}, _From, State) ->
        RoomName = string:trim(RoomNameRaw),
        UserName = string:trim(UserNameRaw),
        Rooms = maps:get(rooms, State, #{}),
        case maps:get(RoomName, Rooms, undefined) of
            undefined ->
                {reply, {error, room_not_found}, State};
            Room ->
                Creator = maps:get(creator, Room),
                case Creator of
                    UserName ->
                        io:format("Room ~s destroyed by ~s~n", [RoomName, UserName]),
                        NewRooms = maps:remove(RoomName, Rooms),
                        {reply, ok, State#{rooms => NewRooms}};
                    _ ->
                        {reply, {error, not_creator}, State}
                end
        end;

handle_call({list_rooms, UserName}, _From, State) ->
    Rooms = maps:get(rooms, State, #{}),
    VisibleRooms = [RoomName || {RoomName, Room} <- maps:to_list(Rooms),
                                    not maps:get(is_private, Room, false) orelse
                                    lists:member(UserName, maps:get(invited_users, Room, []))],
    {reply, VisibleRooms, State};
    
    handle_call({join_room, RoomName, User}, _From, State) ->
        Rooms = maps:get(rooms, State, #{}),
        case maps:get(RoomName, Rooms, undefined) of
            undefined ->
                {reply, {error, room_not_found}, State};
            Room ->
                Users = maps:get(users, Room, []),
                UpdatedRoom = Room#{users => [User | Users]},
                NewRooms = maps:put(RoomName, UpdatedRoom, Rooms),
                {reply, ok, State#{rooms => NewRooms}}
        end;
    
    handle_call({leave_room, RoomName, User}, _From, State) ->
        Rooms = maps:get(rooms, State, #{}),
        case maps:get(RoomName, Rooms, undefined) of
            undefined ->
                {reply, {error, room_not_found}, State};
            Room ->
                Users = lists:delete(User, maps:get(users, Room, [])),
                UpdatedRoom = Room#{users => Users},
                NewRooms = maps:put(RoomName, UpdatedRoom, Rooms),
                {reply, ok, State#{rooms => NewRooms}}
        end;
    
    handle_call({send_message, RoomNameRaw, _Sender, MessageRaw}, _From, State) ->
        RoomName = string:trim(RoomNameRaw),
        Message = string:trim(MessageRaw),
        Rooms = maps:get(rooms, State, #{}),
        case maps:get(RoomName, Rooms, undefined) of
            undefined ->
                {reply, {error, room_not_found}, State};
            Room ->
                Users = maps:get(users, Room, []),
                case is_list(Users) andalso (is_list(Message) orelse is_binary(Message)) of
                    true ->
                        lists:foreach(fun(User) ->
                            io:format("Message to ~s: ~s~n", [User, Message])
                        end, Users),
                        {reply, ok, State};
                    false ->
                        {reply, {error, invalid_data}, State}
                end
        end;
    
    handle_call(_Request, _From, State) ->
        {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.