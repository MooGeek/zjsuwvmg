-module(talk2me_manager).
-behaviour(gen_event).

-include("talk2me_types.hrl").

-export([
    start_link/1
]).
-ignore_xref([
    start_link/1
]).

-export([
    new/1,
    subscribe/2,
    unsubscribe/2,
    send/2,
    users/1
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).


start_link(Name) ->
    gen_event:start_link({via, gproc, {n, l, {?MODULE, Name}}}).

-spec new(iodata())
    -> {ok, identifier()} | {error, _}.
new(Name) ->
    try
	    case supervisor:start_child(talk2me_manager_sup, [Name]) of
            {ok, Pid} ->
                talk2me_bot:attach(Pid, Name),
                {ok, Pid};
            {error, {already_started, Pid}} ->
                {ok, Pid}
        end
    catch
        _:_ ->
            {error, malformed_name}
    end.

-spec subscribe(#room{}, #user{})
    -> gen_event:add_handler_ret() | {error, _}.
subscribe(Room, User) ->
    try
        gen_event:add_sup_handler(Room#room.id, {?MODULE, User#user.id}, [User#user.id])
    catch
        _:_ ->
            {error, malformed_user}
    end.

-spec unsubscribe(#room{}, #user{})
    -> ok.
unsubscribe(Room, User) ->
    try
        gen_event:delete_handler(Room#room.id, {?MODULE, User#user.id}, [User#user.id]),
        UserCount = length( users(Room) ),
        if
            UserCount > 0 -> ok;
            true -> gen_event:stop(Room#room.id)
        end
    catch
        _:_ ->
            ok
    end.

-spec send(#room{}, #message{})
    -> ok.
send(Room, Message) ->
    gen_event:notify(Room#room.id, Message).

-spec users(#room{})
    -> list().
users(Room) ->
    try
        gen_event:which_handlers(Room#room.id)
    catch
        _:_ ->
            []
    end.


-record(state, {
    client :: identifier()
}).

init([Pid]) ->
    {ok, #state{client = Pid}}.

handle_event(Message, State = #state{client = Pid}) ->
    Pid ! {message, Message},
    {ok, State}.

handle_call(_, State) -> 
    {ok, ok, State}.

handle_info(_, State) -> 
    {ok, State}.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
