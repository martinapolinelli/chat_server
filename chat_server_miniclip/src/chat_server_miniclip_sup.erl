%%%-------------------------------------------------------------------
%% @doc chat_server_miniclip top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_miniclip_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 10
    },
    ChildSpecs = [
        {chat_server_miniclip_listener, {chat_server_miniclip_listener, start_link, []}, permanent, 5000, worker, [chat_server_miniclip_listener]},
        {chat_server_miniclip_handler, {chat_server_miniclip_handler, start_link, []}, permanent, 5000, worker, [chat_server_miniclip_handler]},
        {chat_server_miniclip_rooms, {chat_server_miniclip_rooms, start_link, []}, permanent, 5000, worker, [chat_server_miniclip_rooms]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
