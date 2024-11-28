-module(node).
-export([spawn_node/2, add_key/2, get_keys/1,start/3]).
-record(state, {id, keys, predecessor, successor, main}).
-record(node, {id, pid}).

spawn_node(Id, Main) ->
    % io:format("Spawned node: ~p~n", [Id]),
    Pid = spawn(fun() -> start(Id, [], Main) end),
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
            io:format("node_~p.csv created~n", [State#state.id]),
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
            io:format("node: ~p predecessor: ~p successor: ~p~n", [State#state.id, State#state.predecessor, State#state.successor]),
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
    SuccessorId = State#state.successor#node.id,
    PredecessorId = State#state.predecessor#node.id,
    NodeId = State#state.id,
    Data = io_lib:format("~p,~p,~p|~p", [NodeId, PredecessorId, SuccessorId, State#state.keys]), % Data = io_lib:format("~p,~p,~p|~p", [State#state.id, State#state.successor, State#state.predecessor, State#state.keys]),
    FileName = io_lib:format("node_~p.csv", [NodeId]),
    {ok, File} = file:open(FileName, [write]),
    file:write(File, Data),
    file:close(File).

start(Id, Keys, Main) ->
    InitialState = #state{
        id = Id, 
        keys = Keys, 
        main = Main, 
        predecessor = nil, 
        successor = nil
    },
    loop(InitialState).
