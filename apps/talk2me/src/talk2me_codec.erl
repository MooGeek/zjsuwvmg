-module(talk2me_codec).

-include("talk2me_types.hrl").

-export([
    pack/1,
    unpack/1
]).


-spec unpack(iodata())
    -> jiffy:jiffy_decode_result().
unpack(Data) ->
    {Json} = jiffy:decode(Data),
    #message{
        event = proplists:get_value(<<"event">>, Json),
        data = proplists:get_value(<<"data">>, Json)
    }.

-spec pack(#message{})
    -> iodata().
pack(#message{event = Event, data = Data}) ->
    jiffy:encode(#{
        event => Event,
        data => Data
    }).
