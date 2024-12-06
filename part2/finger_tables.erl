-module(finger_tables).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-record(node, {id, non_hashed_id, pid}).


create_finger_tables(NodesLeft,AllNodes,M)->
    GetNext = fun GetNext(Value, Nodes) ->
        case Nodes of
            [Node | Next] ->
                if Value =< Node#node.id -> 
                    Node;
                true -> 
                    GetNext(Value, Next)
                end;
            [] -> 
                hd(AllNodes)
        end
    end,
    case NodesLeft of
        [] ->
            nil;
        [Head | Next] ->
            IndexTable = fingertable_values(Head,M),
            FingerTable = lists:map(fun(Value) -> GetNext(Value, AllNodes) end, IndexTable),
            Head#node.pid ! {set_finger_table, FingerTable},
            create_finger_tables(Next,AllNodes,M)
    end.

fingertable_values(Node,M) ->
    FingerValue = fun(N, K) ->
        ((N + (trunc(math:pow(2, K-1)))) rem (trunc(math:pow(2, M))))
    end,

    N = Node#node.id,
    lists:map(fun(K) -> FingerValue(N, K) end, lists:seq(1, M)).