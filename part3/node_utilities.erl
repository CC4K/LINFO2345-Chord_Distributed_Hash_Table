
-module(node_utilities).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-import(lists,[last/1]).


-record(node, {id, non_hashed_id, pid}).

spawn_nodes(Ids,NodeCount) -> 
    SpawnNodesRecursive = fun SpawnNodesRecursive([CurrNode | Rest]) -> 
    case Rest of
        [] ->
            {NormalID, HashedID} = CurrNode,
            Node = #node{id = HashedID, non_hashed_id = NormalID  ,pid = node:spawn_node(HashedID, NormalID,NodeCount , self())},
            [Node];
        [_| _] ->
            {NormalID, HashedID} = CurrNode,
            Node = #node{id = HashedID, non_hashed_id = NormalID  ,pid = node:spawn_node(HashedID, NormalID,NodeCount , self())},
            [Node | SpawnNodesRecursive(Rest)]
    end
    end,
    Nodes = SpawnNodesRecursive(Ids),
    set_node_predecessor(Nodes),
    Nodes.


set_node_predecessor(Nodes) -> 
    SetNodePredecessorRecursive = fun SetNodePredecessorRecursive(NodesLeft, Predecessor) -> 
        case NodesLeft of
            [] ->
                nil;
            [Current|Next] ->
                if
                    Next == [] ->
                        Current#node.pid ! {set_predecessor, Predecessor};
                    true ->
                        Current#node.pid ! {set_successor, hd(Next)},
                        Current#node.pid ! {set_predecessor, Predecessor},
                        SetNodePredecessorRecursive(Next, Current)
                end
        end
    end,
    FirstNode = hd(Nodes),
    LastNode = last(Nodes),
    SetNodePredecessorRecursive(Nodes, LastNode),
    LastNodePID = LastNode#node.pid,
    LastNodePID ! {set_successor, FirstNode}.

add_node(Node, Nodes, M, BaseN) ->
    NormalId = Node,
    HashedID = hd(main:hash_ids([Node], M)),
    Pid = node:spawn_node(HashedID, NormalId, BaseN , self()),
    NewNode = #node{id = HashedID, non_hashed_id = NormalId, pid = Pid},
    InsertNode = fun InsertNode(NodesLeft) ->
        case NodesLeft of
            [] ->
                [NewNode];
            [CurrentNode | Rest] ->
                if HashedID < CurrentNode#node.id ->
                    [NewNode | NodesLeft];
                true ->
                    [CurrentNode | InsertNode(Rest)]
                end
        end
    end,
    
    NewNodeList = InsertNode(Nodes),
    
    {Predecessor, Successor} = get_neighbors(Nodes, NewNode),
    
    NewNode#node.pid ! {set_predecessor, Predecessor},
    NewNode#node.pid ! {set_successor, Successor},

    Predecessor#node.pid ! {set_successor, NewNode},
    Successor#node.pid ! {set_predecessor, NewNode},
    finger_tables:create_finger_tables(NewNodeList, M),

    NewNodeList.

get_neighbors(Nodes, NewNode) ->
    GetNeighbors = fun GetNeighbors(NodesLeft,PreviousNode) ->
        case NodesLeft of
            [] ->
                nil;
            [CurrentNode | Rest] ->
                if NewNode#node.id < CurrentNode#node.id ->
                    {Predecessor, Successor} = {PreviousNode, CurrentNode},
                    {Predecessor, Successor};
                true ->
                    GetNeighbors(Rest,CurrentNode)
                end
        end
    end,
    NeighBors = GetNeighbors(Nodes,last(Nodes)),
    if NeighBors == nil ->
            {last(Nodes), hd(Nodes)};
        true ->
            NeighBors
    end.






create_nodes(Ids,M,N) ->
    HashedIds = main:hash_ids(Ids, M),
    Normal_and_hashed_ids = lists:zip(Ids, HashedIds),
    Node_IDs = lists:sort(fun({_, A}, {_, B}) -> A =< B end, Normal_and_hashed_ids),
    
    Nodes = spawn_nodes(Node_IDs,N),
    Nodes.

insert_keys(Nodes, Keys) ->
    InsertRemainingKeys = fun InsertRemainingKeys(Node,KeysLeft) ->
        case KeysLeft of
            [Key | NextKey] ->
                Node ! {add_key, Key},
                InsertRemainingKeys(Node, NextKey);
            [] -> 
                nil
        end
    end,
    InsertKeysLoop = fun InsertKeysLoop(NodesLeft, KeysLeft) ->
        case KeysLeft of
            [Key | NextKey] ->
                case NodesLeft of
                    [Node | NextNode] ->
                        PID = Node#node.pid,
                        ID = Node#node.id,
                        if Key =< ID ->
                            PID ! {add_key, Key},
                            InsertKeysLoop([Node | NextNode], NextKey);
                        true ->
                            InsertKeysLoop(NextNode, KeysLeft)
                        end;
                    [] ->
                        KeysLeft
                end;
            [] -> 
                io:fwrite("inserted_all_keys~n"),
                nil
        end
    end,
    RemainingKeys = InsertKeysLoop(Nodes, Keys),
    case RemainingKeys of
        nil ->
            nil;
        _ ->
            case Nodes of
                [Node|_] ->
                    InsertRemainingKeys(Node#node.pid, RemainingKeys);
                [] ->
                    nil
            end
    end.