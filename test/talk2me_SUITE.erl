-module(talk2me_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("talk2me/include/talk2me_types.hrl").

-compile(export_all).

-define(SERVER, "localhost").
-define(PORT, 8888).
-define(PATH, "/websocket").


init_per_suite(_) ->
    {ok, _} = application:ensure_all_started(talk2me),
    {ok, _} = application:ensure_all_started(gun),
    [].

end_per_suite(_) ->
    ok = application:stop(gun),
    ok = application:stop(talk2me).

groups() ->
    [
        {sanity_check, [parallel], [
            codec_sanity
        ]},
        {smoke_test, [parallel, shuffle, {repeat, 5}], [
            basic_flow_smoke, 
            flow_1_smoke, 
            flow_2_smoke, 
            flow_3_smoke, 
            flow_4_smoke, 
            flow_5_smoke
        ]},
        {kinda_unit_test, [sequence], [
            api, 
            manager
        ]}
    ].

all() ->
    [
        {group, sanity_check},
        {group, smoke_test},
        {group, kinda_unit_test}
    ].


%%% 
%%% Sanity checks
%%%

codec_sanity(_) ->
    <<"{\"event\":\"test_event\",\"data\":{\"test\":true}}">> = talk2me_codec:pack(#message{event = <<"test_event">>, data = #{<<"test">> => true}}),
    #message{event = <<"test_event">>, data = {[{<<"test">>,true}]}} = talk2me_codec:unpack(<<"{\"event\":\"test_event\",\"data\":{\"test\":true}}">>).


%%%
%%% Smoke tests
%%%

basic_flow_smoke(_) ->
    {ok, Connection} = connect(?SERVER, ?PATH, ?PORT),
    RegisterMessage = #message{event = <<"user_register">>, data = #{name => ?MODULE, room => ?FUNCTION_NAME}},
    Register = talk2me_codec:pack(RegisterMessage),
    ok = gun:ws_send(Connection, {text, Register}),
    ok = wait_for(gun_ws, Register),
    SendMessage = #message{event = <<"user_message">>, data = <<"Hello world!">>},
    Send = talk2me_codec:pack(SendMessage),
    ok = gun:ws_send(Connection, {text, Send}),
    SendResponse = talk2me_codec:pack(#message{event = <<"user_message">>, data = #{name => ?MODULE, text => <<"Hello world!">>}}),
    ok = wait_for(gun_ws, SendResponse),
    MetaUpdate = talk2me_codec:pack(#message{event = <<"update_meta">>, data = #{users_online => 1}}),
    ok = wait_for(gun_ws, MetaUpdate),
    disconnect(Connection).

flow_1_smoke(_) ->
    {ok, Connection} = connect(?SERVER, ?PATH, ?PORT),
    disconnect(Connection).

flow_2_smoke(_) ->
    {ok, Connection} = connect(?SERVER, ?PATH, ?PORT),
    RegisterMessage = #message{event = <<"user_register">>, data = #{name => ?MODULE, room => ?FUNCTION_NAME}},
    Register = talk2me_codec:pack(RegisterMessage),
    ok = gun:ws_send(Connection, {text, Register}),
    ok = wait_for(gun_ws, Register),
    disconnect(Connection).

flow_3_smoke(_) ->
    {ok, Connection} = connect(?SERVER, ?PATH, ?PORT),
    RegisterMessage = #message{event = <<"user_register">>, data = #{name => ?MODULE, room => ?FUNCTION_NAME}},
    Register = talk2me_codec:pack(RegisterMessage),
    ok = gun:ws_send(Connection, {text, Register}),
    ok = wait_for(gun_ws, Register),
    SendMessage = #message{event = <<"user_message">>, data = <<"Hello world!">>},
    Send = talk2me_codec:pack(SendMessage),
    [gun:ws_send(Connection, {text, Send}) || _ <- lists:seq(1, 10)],
    disconnect(Connection).

flow_4_smoke(_) ->
    [connect(?SERVER, ?PATH, ?PORT) || _ <- lists:seq(1, 10)].

flow_5_smoke(_) ->
    {ok, Connection} = connect(?SERVER, ?PATH, ?PORT),
    Registration = fun() -> RegisterMessage = #message{event = <<"user_register">>, data = #{name => ?MODULE, room => ?FUNCTION_NAME}},
        Register = talk2me_codec:pack(RegisterMessage),
        ok = gun:ws_send(Connection, {text, Register})
    end,
    [Registration() || _ <- lists:seq(1, 10)],
    disconnect(Connection).


%%%
%%% Something like unit tests
%%%

api(_) ->
    {ok, Connection} = connect(?SERVER, ?PATH, ?PORT),
    {Room, User} = talk2me_api:register(?FUNCTION_NAME, ?MODULE),
    #room{name = ?FUNCTION_NAME} = Room,
    #user{name = ?MODULE} = User,
    ok = talk2me_api:send_message(?FUNCTION_NAME, ?MODULE, <<"Hello world!">>),
    SendStruct = #message{event = <<"user_message">>, data = #{name => ?MODULE, text => <<"Hello world!">>}},
    ok = wait_for(message, SendStruct),
    ok = talk2me_api:send_message(Room, ?MODULE, <<"Hello world!">>),
    ok = wait_for(message, SendStruct),
    ok = talk2me_api:send_message(?FUNCTION_NAME, User, <<"Hello world!">>),
    ok = wait_for(message, SendStruct),
    ok = talk2me_api:send_message(Room, User, <<"Hello world!">>),
    ok = wait_for(message, SendStruct),
    ok = talk2me_api:update_meta(Room),
    MetaUpdate = #message{event = <<"update_meta">>, data = #{users_online => 1}},
    ok = wait_for(message, MetaUpdate),
    ok = talk2me_api:update_meta(?FUNCTION_NAME),
    ok = wait_for(message, MetaUpdate),
    disconnect(Connection).
    
manager(_) ->
    {ok, Pid} = talk2me_manager:new(?FUNCTION_NAME),
    {ok, Connection} = connect(?SERVER, ?PATH, ?PORT),
    Room = #room{id = Pid, name = ?FUNCTION_NAME},
    User = #user{id = self(), name = ?MODULE},
    0 = length( talk2me_manager:users(Room) ),
    ok = talk2me_api:send_message(Room, User, <<"Hello world!">>),
    SendStruct = #message{event = <<"user_message">>, data = #{name => ?MODULE, text => <<"Hello world!">>}},
    {error, timeout} = wait_for(message, SendStruct),
    ok = talk2me_manager:subscribe(Room, User),
    1 = length( talk2me_manager:users(Room) ),
    ok = talk2me_api:send_message(Room, User, <<"Hello world!">>),
    ok = wait_for(message, SendStruct),
    ok = talk2me_manager:send(Room, SendStruct),
    ok = wait_for(message, SendStruct),
    ok = talk2me_manager:unsubscribe(Room, User),
    0 = length( talk2me_manager:users(Room) ),
    ok = talk2me_api:send_message(Room, User, <<"Hello world!">>),
    {error, timeout} = wait_for(message, SendStruct),
    disconnect(Connection).


%%%
%%% Internal functions
%%%

connect(Server, Path, Port) ->
    {ok, Connection} = gun:open(Server, Port),
    {ok, _} = gun:await_up(Connection),
    gun:ws_upgrade(Connection, Path),
    ok = wait_for(gun_ws_upgrade, ok),
    {ok, Connection}.

disconnect(Connection) ->
    gun:shutdown(Connection),
    ok.

wait_for(gun_ws_upgrade, Data) ->
    receive
        {gun_ws_upgrade, _, Data, _} ->
            ok
    after
        3000 ->
            {error, timeout}
    end;
wait_for(gun_ws, Data) ->
    receive
        {gun_ws, _, {text, Data}} ->
            ok
    after
        3000 ->
            {error, timeout}
    end;
wait_for(message, Data) ->
    receive
        {message, Data} ->
            ok
    after
        3000 ->
            {error, timeout}
    end.
