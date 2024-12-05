-module(main).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-import(lists,[last/1]).

-record(node, {id, non_hashed_id, pid}).

-define(m, 16).
-define(N, 100).

spawn_main() ->
    Pid = spawn(fun() -> main([]) end),
    Pid.

main(_) -> 
    application:start(crypto),
    io:fwrite("~nstarting up control node...~n"),

    Ids = lists:seq(0, ?N-1),
    % manualy set Ids :
    % Ids = [1,2,3,4,5,6,400,40808,32],
    NodeCount = length(Ids),
    
    Nodes = node_utilities:create_nodes(Ids,?m),

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

    loop(InitialState),
    io:fwrite("DONE~n", []).

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



