-module(main).
-compile(export_all).
-compile(nowarn_export_all).
-import(lists,[last/1]).

-record(state, {nodes, keys}).
-record(node, {id, pid}).


-define(m, 16).
-define(N, 10).



loop(State) -> 
    receive
        {Msg, From} -> 
            Next_State = handle_msg(Msg, From, State),
            loop(Next_State);
        stop -> 
            io:format("Stopping loop~n"),
            ok
    end.


update_state(Key, Value, State) -> 
    NewState = map:put(Key, Value, State),
    NewState.

get_value(Key, State) ->
    map:get(Key, State).

handle_msg(Msg, From, State) -> 
    case Msg of 
        {get_keys, From} -> 
            io:format("Getting keys~n"),
            {ok, From} ! {ok, State}
    end.


main(_) -> 
    application:start(crypto),
    erlang:display("starting up control node"),

    Ids = lists:seq(1, ?N),

    HashedIds = hash_ids(Ids, ?m),
    Node_IDs = lists:sort(HashedIds),
    Nodes = spawn_nodes(Node_IDs),

    File = load_csv("keys.csv"),
    HashedKeys = hash_ids(File, ?m),
    Keys = lists:sort(HashedKeys),
    
    
    
    insert_keys(Nodes, Keys),
    
    InitialState = #{
        nodes => Nodes,
        keys => Keys
    },
    % send message to firtst node
    Node = last(Nodes),
    io:format("Node: ~p~n", [Node]),

    PID = Node#node.pid,
    erlang:display("Sending make_csv to last node"),
    PID ! {make_csv},

    loop(InitialState),
    io:format("DONE ~n", []).


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
            erlang:display("inserted_all_remaining_keys"),
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
            erlang:display("inserted_all_keys"),
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


spawn_nodes_recursive([Id | Rest]) -> 
    case Rest of
        [] ->
            Node = #node{id = Id, pid = node:spawn_node(Id, self())},
            [Node];
        [_| _] ->
            Node = #node{id = Id, pid = node:spawn_node(Id, self())},
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