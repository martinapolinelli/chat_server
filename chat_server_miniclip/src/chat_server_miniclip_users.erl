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
    io:format("LOG: MESSAGE: ~p~n", [Message]),
    io:format("LOG: RECEIVER: ~p~n", [CleanReceiver]),
    io:format("LOG: FINAL SENDER: ~p~n", [Sender]),
    case get_user_pid_and_socket(CleanReceiver) of
        undefined ->
            io:format("LOG: User ~p not found.~n", [CleanReceiver]),
            {error, user_not_found};
        {ReceiverPid, ReceiverSocket} ->
            io:format("LOG: ReceiverPid: ~p~n", [ReceiverPid]),
            io:format("LOG: ReceiverSocket: ~p~n", [ReceiverSocket]),
            ReceiverPid ! {private_message, Sender, Message, ReceiverSocket},
            %ReceiverPid ! {private_message, Sender, Message},
            ok
    end.

get_user_pid_and_socket(UserName) ->
    io:format("LOG: PID AND SOCKT: ~p~n", [UserName]),
    gen_server:call(?MODULE, {get_user_pid_and_socket, UserName}).

handle_call(get_users, _From, State) ->
    {reply, maps:keys(State), State};
handle_call({get_user_pid_and_socket, UserName}, _From, State) ->
    io:format("LOG: Looking up user ~s in State: ~p~n", [UserName, State]),
    case maps:get(UserName, State, undefined) of
        undefined ->
            io:format("LOG: User ~s not found.~n", [UserName]),
            {reply, undefined, State};
        {Pid, Socket} ->
            io:format("LOG: Found user ~s with Pid: ~p and Socket: ~p~n", [UserName, Pid, Socket]),
            {reply, {Pid, Socket}, State}
    end;
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({add_user, UserName, Pid, Socket}, State) ->
    io:format("LOG: Adding user ~s with Pid: ~p and Socket: ~p~n", [UserName, Pid, Socket]),
    NewState = maps:put(UserName, {Pid, Socket}, State),
    io:format("LOG: State after add_user: ~p~n", [NewState]),
    {noreply, NewState};
handle_cast({remove_user, UserName}, State) ->
    io:format("User removed: ~s~n", [UserName]),
    NewState = maps:remove(UserName, State),
    {noreply, NewState};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.