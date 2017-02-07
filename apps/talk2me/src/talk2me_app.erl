%%%-------------------------------------------------------------------
%% @doc talk2me public API
%% @end
%%%-------------------------------------------------------------------

-module(talk2me_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, talk2me, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, talk2me, "static"}},
            {"/websocket", talk2me_handler, []}
        ]}]),
    {ok, _} = cowboy:start_clear(http, 100, [{port, 8888}], #{
        env => #{dispatch => Dispatch}
    }),
    talk2me_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
