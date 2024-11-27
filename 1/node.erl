-module(node).
-export([spawn_node/1, add_key/2, get_keys/1,start/2]).
-record(state, {id, keys}).

spawn_node(Id) ->
    % io:format("Spawned node: ~p~n", [Id]),
    spawn(fun() -> start(Id, []) end).

loop(State) ->
    receive
        {add_key, Key} ->
            % io:format("Node ~p: Added key ~p~n", [State#state.id, Key]),
            NewState = add_key(Key,State),
            loop(NewState);
        {make_csv} ->
            io:format("Node ~p: Making CSV~n", [State#state.id]),
            create_csv(State),
            io:format("Node ~p: Created CSV~n", [State#state.id]),
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


% csv format: key_identifier,contacted_node_identifier1|contacted_node_identifier2|contacted_node_identifier3...
create_csv(State) ->
    FileName = "keysaa.csv",

    io:format("All keys: ~p~n", [State#state.keys]).






start(Id, Keys) ->
    InitialState = #state{id = Id, keys = Keys},
    loop(InitialState).