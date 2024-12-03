-module(main).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-import(lists,[last/1]).

-record(node, {id, non_hashed_id, pid}).

-define(m, 16). %Number of bits in the hash & max entries in finger table
-define(N, 10). %Number of nodes in the ring



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

    Ids = lists:seq(0, ?N-1),
    Nodes = node_creation:create_nodes(Ids,?m),

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
            IndexTable = fingertable_values(Head),
            io:fwrite("IndexTable: ~p~n", [IndexTable]),
            FingerTable = lists:map(fun(Value) -> GetNext(Value, AllNodes) end, IndexTable),
            Head#node.pid ! {set_finger_table, FingerTable},
            io:fwrite("FingerTable: ~p~n", [FingerTable]),
            create_finger_tables(Next,AllNodes)
    end.

fingertable_values(Node) ->
    FingerValue = fun(N, K, M) ->
        ((N + (trunc(math:pow(2, K-1)))) rem (trunc(math:pow(2, M))))
    end,

    N = Node#node.id,
    M = 16,
    lists:map(fun(K) -> FingerValue(N, K, M) end, lists:seq(1, 16)).




create_csvs(Nodes) -> 
    lists:foreach(fun(Node) -> Node#node.pid ! {make_csv} end, Nodes),
    io:fwrite("created all csvs~n").

update_state(Key, Value, State) -> 
    NewState = map:put(Key, Value, State),
    NewState.

get_value(Key, State) ->
    map:get(Key, State).

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



hash_ids(Ids, M) ->
    BinaryToInt = fun(Binary) ->
        lists:foldl(fun(Byte, Acc) -> (Acc bsl 8) bor Byte end, 0, binary:bin_to_list(Binary))
    end,
    MaxValue = 1 bsl M, % 2^m
    lists:map(fun(Id) ->
        Hash = crypto:hash(sha, integer_to_binary(Id)),
        IntegerHash = BinaryToInt(Hash),
        %% Map to range 1 to 2^m
        (IntegerHash rem MaxValue) 
    end, Ids).


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