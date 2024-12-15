-module(chat_server_miniclip_users).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-export([start_link/0, add_user/2, remove_user/1, get_connected_users/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(get_users, _From, State) ->
    {reply, maps:keys(State), State};

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({add_user, UserName, Pid}, State) ->
    NewState = maps:put(UserName, Pid, State),
    {noreply, NewState};

handle_cast({remove_user, UserName}, State) ->
    NewState = maps:remove(UserName, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

add_user(UserName, Pid) ->
    gen_server:cast(?MODULE, {add_user, UserName, Pid}).

remove_user(UserName) ->
    gen_server:cast(?MODULE, {remove_user, UserName}).

get_connected_users() ->
    gen_server:call(?MODULE, get_users).