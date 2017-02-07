%%%-------------------------------------------------------------------
%% @doc talk2me top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(talk2me_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) -> 
    Flags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    ManagerSup = #{
        id => talk2me_manager_sup,
        start => {talk2me_manager_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [talk2me_manager_sup]
    },
    BotSup = #{
        id => talk2me_bot_sup,
        start => {talk2me_bot_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [talk2me_bot_sup]
    },
    {ok, {Flags, [ManagerSup, BotSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
