-module(node).
-export([spawn_node/3, add_key/2, get_keys/1,start/4]).
-record(state, {id, non_hashed_id, keys, predecessor, successor, main}).
-record(node, {id, non_hashed_id, pid}).

spawn_node(Id, NonHashedID, Main) ->
    % io:format("Spawned node: ~p~n", [Id]),
    Pid = spawn(fun() -> start(Id, NonHashedID, [], Main) end),
    % register(Id, Pid),
    Pid.

loop(State) ->
    receive
        {add_key, Key} ->
            % io:format("Node ~p: Added key ~p~n", [State#state.id, Key]),
            NewState = add_key(Key,State),
            loop(NewState);
        {make_csv} ->
            create_csv(State),
            io:format("~p.csv created~n", [State#state.non_hashed_id]),
            loop(State);
        {set_predecessor, Predecessor} ->
            % io:format("Node: ~p predecessor: ~p successor: ~p~n", [State#state.id, Predecessor, State#state.successor]),
            NewState = State#state{predecessor = Predecessor},
            loop(NewState);
        {set_successor, Successor} ->
            % io:format("Node: ~p predecessor: ~p successor: ~p~n", [State#state.id, State#state.predecessor, Successor]),
            NewState = State#state{successor = Successor},
            loop(NewState);
        {print} ->
            io:format("node: ~p predecessor: ~p successor: ~p ~n", [State#state.id, State#state.predecessor, State#state.successor]),
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
    SuccessorId = io_lib:format("~.16B", [State#state.successor#node.id]),
    PredecessorId = io_lib:format("~.16B", [State#state.predecessor#node.id]),
    NodeId = io_lib:format("~.16B", [State#state.id]),
    Keys = lists:map(fun(Number) -> io_lib:format("~.16B", [Number]) end, State#state.keys),
    Data = io_lib:format("~p,~p,~p|~p", [NodeId, PredecessorId, SuccessorId, Keys]),
    FileName = io_lib:format("~p.csv", [State#state.non_hashed_id]),
    {ok, File} = file:open(FileName, [write]),
    file:write(File, Data),
    file:close(File).

start(Id, NonHashedID, Keys, Main) ->
    InitialState = #state{
        id = Id, 
        non_hashed_id = NonHashedID,
        keys = Keys, 
        main = Main, 
        predecessor = nil, 
        successor = nil
    },
    loop(InitialState).
