-module(main).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-import(lists,[last/1]).

-record(node, {id, non_hashed_id, pid}).

-define(m, 16). %Number of bits in the hash & max entries in finger table
-define(N, 6). %Number of nodes in the ring



loop(State) -> 
    receive
        {Msg, From} -> 
            Next_State = handle_msg(Msg, From, State),
            loop(Next_State);
        % {done, From} -> 
        %     NameDir = io_lib:format("dht_~p", [?N]),
        %     csv:create_csvs(From, NameDir),
        %     loop(State);
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
    % manualy set Ids :
    % Ids = [1,2,3,4,5,6,400,40808,32],
    NodeCount = length(Ids),

    Nodes = node_utilities:create_nodes(Ids,?m,NodeCount),

    finger_tables:create_finger_tables(Nodes,Nodes,?m),
    File = csv:load_csv("keys.csv"),
    HashedKeys = hash_ids(File, ?m),
    Keys = lists:sort(HashedKeys),

    node_utilities:insert_keys(Nodes, Keys),
    
    InitialState = #{
        nodes => Nodes,
        keys => Keys
    },
    % creating directory accordingly to the number of nodes
    NameDir = io_lib:format("dht_~p", [NodeCount]),
    file:make_dir(NameDir),
    % saving data in files
    csv:create_csvs(Nodes, NameDir),

    io:fwrite("Nodes: ~p~n", [Nodes]),

    KeyQueriesCSV = csv:load_csv("key_queries.csv"),
    KeyQueries = hash_ids(KeyQueriesCSV, ?m),

    [Node#node.pid ! {find_key, Key} || Node <- Nodes, Key <- KeyQueries],


    NewNodes = node_utilities:add_node(422, Nodes, ?m),
    
    io:fwrite("NewNodes: ~p~n", [NewNodes]),
    
    NewNodes2 = node_utilities:add_node(69, NewNodes, ?m),
    NewNodes3 = node_utilities:add_node(19, NewNodes2, ?m),
    NewNodes4 = node_utilities:add_node(893698, NewNodes3, ?m),

    io:fwrite("NewNodes: ~p~n", [NewNodes4]),
    
loop(InitialState),
    io:fwrite("DONE~n", []).


update_state(Key, Value, State) -> 
    NewState = map:put(Key, Value, State),
    NewState.

get_value(Key, State) ->
    map:get(Key, State).



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

