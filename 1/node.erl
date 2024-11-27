-module(node).
-export([spawn_node/4, add_key/2, get_keys/1,start/5]).
-record(state, {id, keys, predecessor, successor, main}).

spawn_node(Id, Main, Predecessor, Successor) ->
    io:format("Spawned node: ~p predecessor: ~p successor: ~p~n", [Id, Predecessor, Successor]),
    
    spawn(fun() -> start(Id, [], Main, Predecessor, Successor) end).

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
            loop(State);
        {set_predecessor, Predecessor} ->
            io:format("Node: ~p predecessor: ~p successor: ~p~n", [State#state.id, Predecessor, State#state.successor]),
            NewState = State#state{predecessor = Predecessor},
            loop(NewState);
        {set_successor, Successor} ->
            io:format("Node: ~p predecessor: ~p successor: ~p~n", [State#state.id, State#state.predecessor, Successor]),
            NewState = State#state{successor = Successor},
            loop(NewState)
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







start(Id, Keys, Main, Predecessor, Successor) ->
    InitialState = #state{
        id = Id, 
        keys = Keys, 
        main = Main, 
        predecessor = Predecessor, 
        successor = Successor
    },
    loop(InitialState).