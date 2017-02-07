%%%-------------------------------------------------------------------
%% @doc talk2me top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(talk2me_bot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-ignore_xref([start_link/0]).

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
        strategy => simple_one_for_one,
        intensity => 10,
        period => 10
    },
    Child = #{
        id => talk2me_bot,
        start => {talk2me_bot, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [talk2me_bot]
    },
    {ok, {Flags, [Child]}}.

%%====================================================================
%% Internal functions
%%====================================================================
