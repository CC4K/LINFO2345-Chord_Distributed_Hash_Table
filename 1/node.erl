-module(node).
-export([spawn_node/1, add_key/2, get_keys/1,start/2]).
-record(state, {id, keys}).

spawn_node(Id) ->
    io:format("Spawned node: ~p~n", [Id]),
    spawn(fun() -> start(Id, []) end).

loop(State) ->
    receive
        {add_key, Key} ->
            io:format("Node ~p: Added key ~p~n", [State#state.id, Key]),
            NewState = add_key(Key,State),
            loop(NewState);
        {make_csv, _} ->
            create_csv(State),
            loop(State)
    end.


add_key(Key, State) ->
    NewState = State#state{keys = [Key|State#state.keys]},
    NewState.


get_keys(Pid) ->
    Pid ! {get_keys, self()},
    receive
        {keys, Keys} -> Keys
    end.

create_csv(State) ->
    FileName = "keys.csv",
    Data = lists:map(fun(Key) -> Key end, State#state.keys),
    file:write_file(FileName, Data).

start(Id, Keys) ->
    InitialState = #state{id = Id, keys = Keys},
    loop(InitialState).