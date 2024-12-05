-module(node).
-export([spawn_node/3]).
-record(state, {id, non_hashed_id, keys, predecessor, successor, main, fingertable, keys_path}).
-record(node, {id, non_hashed_id, pid}).
-record(keys_path, {keys_path, keys_left_to_find}).
-record(key_path, {key, path}).
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
        {make_csv, NameDir} ->
            csv:create_node_csv(State, NameDir),
            % io:format("~p.csv created~n", [State#state.non_hashed_id]),
            loop(State);
        {set_predecessor, Predecessor} ->
            % io:format("Node: ~p predecessor: ~p successor: ~p~n", [State#state.id, Predecessor, State#state.successor]),
            NewState = State#state{predecessor = Predecessor},
            loop(NewState);
        {set_successor, Successor} ->
            % io:format("Node: ~p predecessor: ~p successor: ~p~n", [State#state.id, State#state.predecessor, Successor]),
            NewState = State#state{successor = Successor},
            loop(NewState);
        {set_finger_table, FingerTable} ->
            NewState = State#state{fingertable = FingerTable},
            % io:format("Node: ~p finger table: ~p~n", [State#state.id, FingerTable]),
            loop(NewState);
        {lookup_key,Caller,Key, Chain} -> %TODO: make it so that the first node isn't in the list in the CSV
            % io:format("Node ~p: Looking up key ~p PID: ~p~n", [State#state.id, Key, self()]),
            lookup_key(State,Key,Caller, Chain),
            loop(State);
        {found_key, Key, Chain} ->
            % io:format("Key ~p found at node ~p with chain ~p~n", [Key, ResultNode#node.non_hashed_id, Chain]),
            KeyPath = #key_path{key = Key, path = Chain},
            KeysPath = State#state.keys_path,
            NewKeysPath = KeysPath#keys_path{keys_path = [KeyPath|KeysPath#keys_path.keys_path]},
            io:format("Node ~p: Keys path ~p~n", [State#state.id, NewKeysPath]),
            NewState = State#state{keys_path = NewKeysPath},
            loop(NewState);
        {print} ->
            io:format("node: ~p predecessor: ~p successor: ~p ~n", [State#state.id, State#state.predecessor, State#state.successor]),
            loop(State)
    end.
    
    
    get_current_node(State)->
        CurrentNode = #node{id = State#state.id, non_hashed_id = State#state.non_hashed_id, pid = self()},
        CurrentNode.        


lookup_key(State, Key, Caller, Chain)->
    CurrentNode = get_current_node(State),
    % CurrentId = CurrentNode#node.id,
    FingerTable = State#state.fingertable,
    LookupKeyLoop = fun LookupKeyLoop(LastNode,NodesLeft,FirstJump)->
        case NodesLeft of
            [Head|Tail]->
                if Head#node.id == LastNode#node.id ->
                    %^ EQUAL
                LookupKeyLoop(Head, Tail,0);
                Head#node.id < LastNode#node.id ->
                        %^ OVERFLOW
                        CurrBigger = Head#node.id > Key,
                        LastSmaller = LastNode#node.id < Key,
                        if CurrBigger or LastSmaller ->
                            io:fwrite("OVERFLOW LastNode: ~p ~n", [LastNode#node.id]),
                            {LastNode, FirstJump};
                        true ->
                            LookupKeyLoop(Head,Tail,0)
                        end;
                    true ->
                        %^ NORMAL CASE
                        CurrBigger = Head#node.id > Key,
                        LastSmaller = LastNode#node.id < Key,
                        if CurrBigger and LastSmaller->
                            io:fwrite("NORMAL LastNode: ~p ~n", [LastNode#node.id]),
                            {LastNode, FirstJump};
                        true -> 
                            LookupKeyLoop(Head,Tail,0)
                        end
                end;
            []->
                io:fwrite("END LastNode: ~p ~n", [LastNode#node.id]),
                {LastNode, FirstJump}
        end
    end,
    io:fwrite("Current node: ~p Key: ~p ", [CurrentNode#node.id, Key]),
    
    % io:fwrite("finger table: ~p~n", [FingerTable]),
    if CurrentNode#node.id == 65103 ->
    io:fwrite("Finger table for node 65103: ~p~n", [FingerTable]);
    true ->
        ok
    end,



    {ResultNode,FirstJump} = LookupKeyLoop(CurrentNode,FingerTable,1),

    if FirstJump == 1 ->
            Caller ! {found_key, Key, Chain ++ [CurrentNode|hd(FingerTable)]};
        true ->
        ResultNode#node.pid ! {lookup_key, Caller, Key, Chain ++ [CurrentNode]}
    end.
add_key(Key, State) ->
    NewState = State#state{keys = [Key|State#state.keys]},
    NewState.

get_keys(Pid) ->
    Pid ! {get_keys, self()},
    receive
        {keys, Keys} -> Keys
    end.



start(Id, NonHashedID, Keys, Main) ->
    KeysPath = #keys_path{keys_path = [], keys_left_to_find = 0},
    InitialState = #state{
        id = Id, 
        non_hashed_id = NonHashedID,
        keys = Keys, 
        main = Main, 
        predecessor = nil, 
        successor = nil,
        keys_path = KeysPath
    },
    loop(InitialState).
