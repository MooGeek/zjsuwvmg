-module(talk2me_bot).
-behaviour(gen_statem).

-define(NAME, <<"vovka@never_ever_tsardom">>).
-define(MAX_DELAY, 10000).

-export([
    attach/2
]).

-export([
    start_link/2
]).
-ignore_xref([
    start_link/2
]).

-export([
    init/1,
    terminate/3,
    code_change/4,
    callback_mode/0
]).

-export([
    handle_event/4
]).

-record(data, {
    from    :: identifier(),
    room    :: iodata(),
    name    :: iodata()
}).

-spec attach(identifier(), iodata())
    -> supervisor:startchild_ret().
attach(From, Room) ->
    supervisor:start_child(talk2me_bot_sup, [From, Room]).


start_link(From, Room) -> 
    gen_statem:start_link(?MODULE, [From, Room], []).


init([From, Room]) ->
    process_flag(trap_exit, true),
    link(From),
    {ok, active, #data{from = From, room = Room, name = ?NAME}, {next_event, internal, undefined}}.

callback_mode() ->
    handle_event_function.


handle_event(info, {'EXIT', From, _}, _, #data{from = From}) ->
    {stop, normal};
handle_event(_, _, _, Data = #data{room = Room, name = Name}) ->
    talk2me_api:send_message(Room, Name, get_quote()),
    {keep_state, Data, rand:uniform(?MAX_DELAY)}.


terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.


-spec get_quote()
    -> iodata().
get_quote() ->
    Vocabulary = vocabulary(),
    RandomIndex = rand:uniform( length(Vocabulary) ),
    lists:nth(RandomIndex, Vocabulary).

-spec vocabulary()
    -> list().
vocabulary() ->
    [
        <<"И так сойдет!"/utf8>>,
        <<"Вы че? А, это вы и пальцы за меня загибать будете?"/utf8>>,
        <<"Эй, эй! Алё! стоп! Вы что это? И конфеты за меня есть будете?"/utf8>>,
        <<"Не хочу, не хочу! В школе учат, учат, еще и тут, в сказке навалились!"/utf8>>,
        <<"Эх, вот бы мне бы тоже бы научиться по обмену какими-нибудь премудростями..."/utf8>>,
        <<"Эх, мне бы только это суметь. Она бы сейчас весь рот открыла!"/utf8>>,
        <<"Чего ты? Куда ты меня тащишь? Ух ты, подумаешь! Килька несчастная!"/utf8>>,
        <<"Ну, вот еще! Снова здорово! Сначала тебе корыто, потом тебе подавай стиральную машину..."/utf8>>,
        <<"Ничего вы не понимаете в царской жизни! Царь! Ха! Хочешь – пирожное, хочешь – мороженое… А он, ха-ха, заборы красит!"/utf8>>,
        <<"Все сам, да сам, а тут ведь вот — царская жизнь. Только и делай, что ничего не делай."/utf8>>,
        <<"Щас как я все это замесю!!!"/utf8>>,
        <<"А чего это, теста? А что она такая липкая? А ладно, зажарится как-нибудь!"/utf8>>,
        <<"Эээй, златая рыбка!!! Ты что, не слышишь, что ли?!"/utf8>>
    ].
