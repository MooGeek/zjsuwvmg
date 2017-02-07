-module(talk2me_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("talk2me/include/talk2me_types.hrl").

-compile(export_all).

-define(SERVER, "localhost").
-define(PORT, 8888).
-define(SOCKET, "/websocket").

init_per_suite(_) ->
    {ok, _} = application:ensure_all_started(talk2me),
    {ok, _} = application:ensure_all_started(gun),
    [].

end_per_suite(_) ->
    ok = application:stop(gun),
    ok = application:stop(talk2me).

all() ->
    [
        test
    ].


test(_) ->
    {ok, Connection} = gun:open(?SERVER, ?PORT),
    {ok, _} = gun:await_up(Connection),
    Stream = gun:ws_upgrade(Connection, ?SOCKET),
    receive
        {gun_ws_upgrade, Pid, ok, _} ->
            ok;
        Msg ->
            ct:print("Unexpected message ~p", [Msg])
    end,
    ok = gun:ws_send(Connection, {text, <<"{\"event\":\"user_register\", \"data\": {\"name\":\"user\", \"room\":\"test\"}}">>}),
    receive
        Msg2 ->
            ct:print("~p", [Msg2])
    end,
    ok = gun:ws_send(Connection, {text, <<"{\"event\": \"user_message\", \"data\": \"test message\"}">>}),
    receive
        Msg3 ->
            ct:print("~p", [Msg3])
    end,
    gun:shutdown(Connection).
