-module(talk2me_handler).

-include("talk2me_types.hrl").

-define(PING_INTERVAL, 1000).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).
-ignore_xref([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).


-record(state, {
    user    :: tuple() | undefined,
    room    :: tuple() | undefined,
    pinger  :: timer:tref() | undefined
}).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(_) ->
    event(<<"user_connect">>, undefined, #state{}).

websocket_handle({text, Data}, State) ->
    Message = talk2me_codec:unpack(Data),
    websocket_handle({message, Message}, State);
websocket_handle({message, Message}, State) ->
    event(Message#message.event, Message#message.data, State);
websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info(ping, State) ->
    event(<<"ping">>, undefined, State);
websocket_info({message, Message}, State) ->
    {reply, {text, talk2me_codec:pack(Message)}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_, _, #state{user = User, room = Room, pinger = Pinger}) ->
    stop_ping(Pinger),
    talk2me_manager:unsubscribe(Room, User),
	ok;
terminate(_, _, _) ->
    ok.


-spec start_ping() 
    -> {ok, timer:tref()} | {error, _}.
start_ping() ->
        timer:send_interval(?PING_INTERVAL, ping).

-spec stop_ping(timer:tref())
    -> {ok, cancel} | {error, _}.
stop_ping(Pinger) ->
        timer:cancel(Pinger).

-spec event(iodata(), any(), State)
    -> {ok, State}
    when State::#state{}.
event(<<"ping">>, _, State = #state{room = Room}) ->
    talk2me_api:update_meta(Room),
    {ok, State};
event(<<"user_connect">>, _, State) ->
    {ok, State};
event(<<"user_disconnect">>, _, State) ->
    {ok, State};
event(<<"user_register">>, Data, State = #state{user = undefined}) ->
    {RegisterInfo} = Data,
    UserName = proplists:get_value(<<"name">>, RegisterInfo, <<>>),
    RoomName = proplists:get_value(<<"room">>, RegisterInfo, <<>>),
    if
        UserName =:= <<>> ->
            {ok, State};
        RoomName =:= <<>> ->
            {ok, State};
        true ->
            {Room, User} = talk2me_api:register(RoomName, UserName),
            {ok, Timer} = start_ping(),
            State2 = #state{
                user = User,
                room = Room,
                pinger = Timer
            },
            {ok, State2}
    end;
event(<<"user_message">>, Data, State = #state{user = User, room = Room}) ->
    talk2me_api:send_message(Room, User, Data),
    {ok, State};
event(_, _, State) ->
    {ok, State}.
