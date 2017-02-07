-record(room, {
    id                      :: identifier(),
    name = <<"default">>    :: iodata()
}).

-record(user, {
    id                      :: identifier(),
    name = <<"anonymous">>  :: iodata()
}).

-record(message, {
    event   :: iodata(),
    data    :: term()
}).
