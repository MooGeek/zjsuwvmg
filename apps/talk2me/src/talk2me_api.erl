-module(talk2me_api).

-include("talk2me_types.hrl").

-export([
    register/2,
    send_message/3,
    update_meta/1
]).

-spec room(iodata())
    -> #room{}.
room(RoomName) ->
    {ok, Id} = talk2me_manager:new(RoomName),
    #room{
        id = Id,
        name = RoomName
    }.

-spec user(iodata())
    -> #user{}.
user(UserName) ->
    #user{
        id = self(),
        name = UserName
    }.

-spec users_online(#room{})
    -> integer().
users_online(Room) ->
    length( talk2me_manager:users(Room) ).

-spec register(iodata(), iodata())
    -> {#room{}, #user{}}.
register(RoomName, UserName) ->
    Room = room(RoomName),
    User = user(UserName),
    ok = talk2me_manager:subscribe(Room, User),
    Message = #message{
        event = <<"user_register">>,
        data = #{room => RoomName}
    },
    talk2me_manager:send(Room, Message),
    {Room, User}.

-spec send_message(Room, User, iodata())
    -> ok
    when Room::#room{} | iodata(),
         User::#user{} | iodata().
send_message(#room{name = RoomName}, UserName, Text) ->
    send_message(RoomName, UserName, Text);
send_message(RoomName, #user{name = UserName}, Text) ->
    send_message(RoomName, UserName, Text);
send_message(RoomName, UserName, Text) ->
    Room = room(RoomName),
    Message = #message{
        event = <<"user_message">>,
        data = #{
            name => UserName,
            text => Text
        }
    },
    talk2me_manager:send(Room, Message).

-spec update_meta(#room{} | iodata())
    -> ok.
update_meta(#room{name = RoomName}) ->
    update_meta(RoomName);
update_meta(RoomName) ->
    Room = room(RoomName),
    Message = #message{
        event = <<"update_meta">>,
            data = #{
                users_online => users_online(Room)
            }
    },
    talk2me_manager:send(Room, Message).
