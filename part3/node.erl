-module(node).
-export([spawn_node/4]).
-record(state, {id, non_hashed_id, keys, predecessor, successor, main, fingertable, keys_path, node_count}).
-record(node, {id, non_hashed_id, pid}).
-record(keys_path, {keys_path, keys_left_to_find}).
-record(key_path, {key, path}).
spawn_node(Id, NonHashedID, NodeCount, Main) ->
    Pid = spawn(fun() -> start(Id, NonHashedID, NodeCount, [], Main) end),
    Pid.

loop(State) ->
    receive
        {add_key, Key} ->
            NewState = add_key(Key,State),
            loop(NewState);
        {make_csv, NameDir} ->
            csv:create_node_csv(State, NameDir),
            loop(State);
        {make_queries_csv, NameDir} ->
            csv:create_queries_csv(State, NameDir),
            loop(State);
        {set_predecessor, Predecessor} ->
            if State#state.predecessor == nil ->
                    NewState = State#state{predecessor = Predecessor},
                    loop(NewState);
                true ->
                    NewKeys = transfer_keys_to_predecessor(State, Predecessor),
                    NewState = State#state{predecessor = Predecessor, keys = NewKeys},
                    loop(NewState)
            end;
        {set_successor, Successor} ->
            NewState = State#state{successor = Successor},
            loop(NewState);
        {set_finger_table, FingerTable} ->
            KeysPath = #keys_path{keys_path = [], keys_left_to_find = 0},
            NewState = State#state{fingertable = FingerTable, keys_path = KeysPath},
            loop(NewState);
        {find_key,Key} ->
            case own_key(State, Key) of
                true ->
                    self() ! {found_key, Key, [get_current_node(State)]},
                    loop(State);
                false ->
                    lookup_key(State,Key,self(),[]),
                    loop(State)
            end;

        {lookup_key,Caller,Key, Chain} ->
            lookup_key(State,Key,Caller, Chain),
            loop(State);
        {found_key, Key, Chain} ->
            KeyPath = #key_path{key = Key, path = Chain},
            KeysPath = State#state.keys_path,
            NewKeysPath = KeysPath#keys_path{keys_path = [KeyPath|KeysPath#keys_path.keys_path]},
            NewState = State#state{keys_path = NewKeysPath},
            NameDir = io_lib:format("dht_~p", [NewState#state.node_count]),
            csv:create_queries_csv(NewState, NameDir),
            loop(NewState);
        {print} ->
            loop(State)
    end.
    
    

transfer_keys_to_predecessor(State, Predecessor) ->
    PredecessorId = Predecessor#node.id,
    Id = State#state.id,
    LoopNormal = fun LoopNormal(KeysLeft) -> 
        case KeysLeft of
            [Key | Next] ->
                if Key > Id ->
                    Predecessor#node.pid ! {add_key, Key},
                    LoopNormal(Next);
                    true ->
                        if Key > PredecessorId ->
                                [Key | LoopNormal(Next)];
                            true ->
                                Predecessor#node.pid ! {add_key, Key},
                                LoopNormal(Next)
                        end
                end;
            [] ->
                []
        end
    end,
    LoopOverflow = fun LoopOverflow(KeysLeft) -> 
        case KeysLeft of
            [Key | Next] ->
                if Key < Id ->
                    [Key | LoopOverflow(Next)];
                    true ->
                        if Key > PredecessorId ->
                                [Key | LoopOverflow(Next)];
                            true ->
                                Predecessor#node.pid ! {add_key, Key},
                                LoopOverflow(Next)
                        end
                end;
            [] ->
                []
        end
    end,
    
    NewKeys = if PredecessorId < Id ->
            LoopNormal(State#state.keys);
        true ->
            LoopOverflow(State#state.keys)
    end,

    % Make CSV is called here since messages from node A to B are guaranteed to arrive in order
    Predecessor#node.pid ! {make_csv, io_lib:format("dht_~p", [State#state.node_count])},

    NewKeys.


get_current_node(State)->
    CurrentNode = #node{id = State#state.id, non_hashed_id = State#state.non_hashed_id, pid = self()},
    CurrentNode.        


lookup_key(State, Key, Caller, Chain)->
    CurrentNode = get_current_node(State),
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
                            {LastNode, FirstJump};
                        true ->
                            LookupKeyLoop(Head,Tail,0)
                        end;
                    true ->
                        %^ NORMAL CASE
                        CurrBigger = Head#node.id > Key,
                        LastSmaller = LastNode#node.id < Key,
                        if CurrBigger and LastSmaller->
                            {LastNode, FirstJump};
                        true -> 
                            LookupKeyLoop(Head,Tail,0)
                        end
                end;
            []->
                {LastNode, FirstJump}
        end
    end,

    {ResultNode,FirstJump} = LookupKeyLoop(CurrentNode,FingerTable,1),

    if FirstJump == 1 ->
            Caller ! {found_key, Key, Chain ++ [CurrentNode] ++ [hd(FingerTable)]};
        true ->
        ResultNode#node.pid ! {lookup_key, Caller, Key, Chain ++ [CurrentNode]}
    end.
add_key(Key, State) ->
    NewState = State#state{keys = [Key|State#state.keys]},
    NewState.

own_key(State, Key) ->
    Id = State#state.id,
    Predecessor = State#state.predecessor#node.id,
    if Id < Predecessor  ->
            CurrBigger = Id > Key,
            LastSmaller = Predecessor < Key,
            if CurrBigger orelse LastSmaller ->
                true;
            true ->
                false
            end;
        true ->
            CurrBigger = Id > Key,
            LastSmaller = Predecessor < Key,
            if CurrBigger andalso LastSmaller ->
                true;
            true ->
                false
            end
    end.

start(Id, NonHashedID, NodeCount, Keys, Main) ->
    KeysPath = #keys_path{keys_path = [], keys_left_to_find = 0},
    InitialState = #state{
        id = Id, 
        non_hashed_id = NonHashedID,
        keys = Keys, 
        main = Main, 
        predecessor = nil, 
        successor = nil,
        keys_path = KeysPath,
        node_count = NodeCount
    },
    loop(InitialState).
