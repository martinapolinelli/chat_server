-module(chat_server_miniclip_users).
-behaviour(gen_server).

-export([start_link/0, add_user/3, remove_user/1, get_connected_users/0, send_private_message/3, get_user_pid_and_socket/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

add_user(UserName, Pid, Socket) ->
    gen_server:cast(?MODULE, {add_user, UserName, Pid, Socket}).

remove_user(UserName) ->
    gen_server:cast(?MODULE, {remove_user, UserName}).

get_connected_users() ->
    gen_server:call(?MODULE, get_users).

send_private_message(Sender, Receiver, Message) ->
    CleanReceiver = string:trim(Receiver),
    case get_user_pid_and_socket(CleanReceiver) of
        undefined ->
            {error, user_not_found};
        {ReceiverPid, ReceiverSocket} ->
            ReceiverPid ! {private_message, Sender, Message, ReceiverSocket},
            ok
    end.

get_user_pid_and_socket(UserName) ->
    gen_server:call(?MODULE, {get_user_pid_and_socket, UserName}).

handle_call(get_users, _From, State) ->
    {reply, maps:keys(State), State};
handle_call({get_user_pid_and_socket, UserName}, _From, State) ->
    case maps:get(UserName, State, undefined) of
        undefined ->
            {reply, undefined, State};
        {Pid, Socket} ->
            {reply, {Pid, Socket}, State}
    end;
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({add_user, UserName, Pid, Socket}, State) ->
    NewState = maps:put(UserName, {Pid, Socket}, State),
    {noreply, NewState};
handle_cast({remove_user, UserName}, State) ->
    io:format("User removed: ~s~n", [UserName]),
    NewState = maps:remove(UserName, State),
    {noreply, NewState};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.