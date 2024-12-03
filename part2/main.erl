-module(main).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-import(lists,[last/1]).

-record(node, {id, non_hashed_id, pid, fingertable}).

-define(m, 16). %Number of bits in the hash & max entries in finger table
-define(N, 100). %Number of nodes in the ring



loop(State) -> 
    receive
        {Msg, From} -> 
            Next_State = handle_msg(Msg, From, State),
            loop(Next_State);
        stop -> 
            io:format("Stopping loop~n"),
            ok
    end.

handle_msg(Msg, From, State) -> 
    case Msg of 
        {get_keys, From} -> 
            io:format("Getting keys~n"),
            {ok, From} ! {ok, State}
    end.

main(_) -> 
    application:start(crypto),
    io:fwrite("~nstarting up control node...~n"),

    Nodes = create_nodes(?N),
    create_finger_tables(Nodes,Nodes),
    File = load_csv("keys.csv"),
    HashedKeys = hash_ids(File, ?m),
    Keys = lists:sort(HashedKeys),
    % io:fwrite("Keys: ~p~n", [Keys]),

    insert_keys(Nodes, Keys),
    
    InitialState = #{
        nodes => Nodes,
        keys => Keys
    },
    % send message to firtst node
    % Node = last(Nodes),

    create_csvs(Nodes),
    % PID = Node#node.pid,
    % io:fwrite("sending make_csv to last node~n"),
    % PID ! {make_csv},

    loop(InitialState),
    io:fwrite("DONE~n", []).


create_finger_tables(NodesLeft,AllNodes)->
    erlang:display("a"),
    case NodesLeft of
        [] ->
            nil;
        [Head | Next] ->
            erlang:display("b"),
            IndexTable = fingertable_values(Head),
            FingerTable = lists:map(fun(Value) -> get_next(Value, AllNodes) end, IndexTable),
            io:fwrite("FingerTable: ~p~n", [FingerTable]),
            create_finger_tables(Next,AllNodes)
    end.

get_next(Value, Nodes) ->
    case Nodes of
        [Node | Next] ->
            if Value =< Node#node.id -> 
                Node;
            true -> 
                get_next(Value, Next)
            end;
        [] -> 
            hd(Nodes)
    end.


finger_value(N, K, M) ->
    (N + (trunc(math:pow(2, K-1)) rem trunc(math:pow(2, M)))).

% fingertable(1) for [2, 3, 5, 9,...]
fingertable_values(Node) ->
    N = Node#node.id,
    M = 16,
    lists:map(fun(K) -> finger_value(N, K, M) end, lists:seq(1, 16)).


create_nodes(Count) ->
    Ids = lists:seq(1, Count),
    HashedIds = hash_ids(Ids, ?m),
    Normal_and_hashed_ids = lists:zip(Ids, HashedIds),
    Node_IDs = lists:sort(fun({_, A}, {_, B}) -> A =< B end, Normal_and_hashed_ids),
    % io:fwrite("Node IDs: ~p~n", [Node_IDs]),
    
    Nodes = spawn_nodes(Node_IDs),
    Nodes.

create_csvs(Nodes) -> 
    lists:foreach(fun(Node) -> Node#node.pid ! {make_csv} end, Nodes),
    io:fwrite("created all csvs~n").

update_state(Key, Value, State) -> 
    NewState = map:put(Key, Value, State),
    NewState.

get_value(Key, State) ->
    map:get(Key, State).

insert_keys(Nodes, Keys) -> 
    RemainingKeys = insert_keys_loop(Nodes, Keys),
    case RemainingKeys of
        nil ->
            nil;
        _ ->
            case Nodes of
                [Node|_] ->
                    insert_remaining_keys(Node#node.pid, RemainingKeys);
                [] ->
                    nil
            end
    end.

insert_remaining_keys(Node,Keys) ->
    case Keys of
        [Key | NextKey] ->
            Node ! {add_key, Key},
            insert_remaining_keys(Node, NextKey);
        [] -> 
            nil
    end.

insert_keys_loop(Nodes, Keys) ->
    % [{ID, PID} | NextNode]
    % [Key | NextKey]
    
    case Keys of
        [Key | NextKey] ->
            case Nodes of
                [Node | NextNode] ->
                    PID = Node#node.pid,
                    ID = Node#node.id,
                    % io:format("Node: ~p, Key: ~p~n", [ID, Key]),
                    if Key =< ID ->
                        PID ! {add_key, Key},
                        insert_keys_loop([Node | NextNode], NextKey);
                    true ->
                        insert_keys_loop(NextNode, Keys)
                    end;
                [] ->
                    Keys
            end;
        [] -> 
            io:fwrite("inserted_all_keys~n"),
            nil
    end.

spawn_nodes(Ids) -> 
    Nodes = spawn_nodes_recursive(Ids),
    FirstNode = hd(Nodes),
    LastNode = last(Nodes),
    set_node_predecessor_successor_recursive(Nodes, LastNode),
    LastNodePID = LastNode#node.pid,
    LastNodePID ! {set_successor, FirstNode},
    % LastNodePID ! {print},
    Nodes.

set_node_predecessor_successor_recursive(Nodes, Last) -> 
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
                    % Current#node.pid ! {print},
                    set_node_predecessor_successor_recursive(Next, Current)
            end
    end.

spawn_nodes_recursive([CurrNode | Rest]) -> 
    case Rest of
        [] ->
            {NormalID, HashedID} = CurrNode,
            Node = #node{id = HashedID, non_hashed_id = NormalID  ,pid = node:spawn_node(HashedID, NormalID , self())},
            [Node];
        [_| _] ->
            {NormalID, HashedID} = CurrNode,
            Node = #node{id = HashedID, non_hashed_id = NormalID  ,pid = node:spawn_node(HashedID, NormalID , self())},
            [Node | spawn_nodes_recursive(Rest)]
    end.

hash_ids(Ids, M) ->
    MaxValue = 1 bsl M, % 2^m
    lists:map(fun(Id) ->
        Hash = crypto:hash(sha, erlang:term_to_binary(Id)),
        IntegerHash = binary_to_int(Hash),
        %% Map to range 1 to 2^m
        (IntegerHash rem MaxValue) + 1
    end, Ids).

binary_to_int(Binary) ->
    lists:foldl(fun(Byte, Acc) -> (Acc bsl 8) bor Byte end, 0, binary:bin_to_list(Binary)).

sha1(Data) ->
    crypto:hash(sha, Data).

load_csv(FileName) ->
    Lines = readlines(FileName),
    lists:map(fun(Line) -> list_to_integer(string:strip(Line, right, $\n)) end, string:tokens(Lines, "\n")).

% readlines/get_all_lines source -> https://stackoverflow.com/questions/2475270/how-to-read-the-contents-of-a-file-in-erlang
readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
        after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.