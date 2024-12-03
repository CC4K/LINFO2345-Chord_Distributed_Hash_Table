
-module(node_utilities).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-import(lists,[last/1]).


-record(node, {id, non_hashed_id, pid}).

spawn_nodes(Ids) -> 
    SpawnNodesRecursive = fun SpawnNodesRecursive([CurrNode | Rest]) -> 
    case Rest of
        [] ->
            {NormalID, HashedID} = CurrNode,
            Node = #node{id = HashedID, non_hashed_id = NormalID  ,pid = node:spawn_node(HashedID, NormalID , self())},
            [Node];
        [_| _] ->
            {NormalID, HashedID} = CurrNode,
            Node = #node{id = HashedID, non_hashed_id = NormalID  ,pid = node:spawn_node(HashedID, NormalID , self())},
            [Node | SpawnNodesRecursive(Rest)]
    end
    end,
    SetNodePredecessor = fun SetNodePredecessor(Nodes, Last) -> 
        case Nodes of
            [] ->
                nil;
            [Current|Next] ->
                if
                    Next == [] ->
                        Current#node.pid ! {set_predecessor, Last};
                    true ->
                        Current#node.pid ! {set_successor, hd(Next)},
                        Current#node.pid ! {set_predecessor, Last},
                        SetNodePredecessor(Next, Current)
                end
        end
    end,
    Nodes = SpawnNodesRecursive(Ids),
    FirstNode = hd(Nodes),
    LastNode = last(Nodes),
    SetNodePredecessor(Nodes, LastNode),
    LastNodePID = LastNode#node.pid,
    LastNodePID ! {set_successor, FirstNode},
    Nodes.


create_nodes(Ids,M) ->
    HashedIds = main:hash_ids(Ids, M),
    Normal_and_hashed_ids = lists:zip(Ids, HashedIds),
    Node_IDs = lists:sort(fun({_, A}, {_, B}) -> A =< B end, Normal_and_hashed_ids),
    
    Nodes = spawn_nodes(Node_IDs),
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