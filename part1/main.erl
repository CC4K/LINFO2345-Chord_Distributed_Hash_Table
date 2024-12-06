-module(main).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-import(lists,[last/1]).

-record(node, {id, non_hashed_id, pid}).
-record(state, {nodes, name_dir}).

-define(m, 16).
-define(N, 10).

spawn_main() ->
    Pid = spawn(fun() -> main([]) end),
    Pid.

loop(State) -> 
    receive
        {add_keys, Keys} ->
            io:format("Adding keys~n"),
            HashedKeys = hash_ids(Keys, ?m),
            node_utilities:insert_keys(State#state.nodes,HashedKeys),
            csv:create_csvs(State#state.nodes, State#state.name_dir),
            loop(State);
        stop -> 
            io:format("Stopping loop~n"),
            ok
    end.

main(_) -> 
    Ids = lists:seq(0, ?N-1),
    % manualy set Ids :
    % Ids = [1,2,3,4,5,6,400,40808,32],
    start(Ids).

start(Ids) -> 
    application:start(crypto),
    io:fwrite("~nstarting up control node...~n"),

    NodeCount = length(Ids),
    
    Nodes = node_utilities:create_nodes(Ids,?m),

    File = csv:load_csv("keys.csv"),
    HashedKeys = hash_ids(File, ?m),
    Keys = lists:sort(HashedKeys),
    
    node_utilities:insert_keys(Nodes, Keys),
    


    % creating directory accordingly to the number of nodes
    NameDir = io_lib:format("dht_~p", [NodeCount]),
    case filelib:is_dir(NameDir) of
        true ->
            file:del_dir_r(NameDir);
        false ->
            ok
    end,

    file:make_dir(NameDir),
    csv:create_csvs(Nodes, NameDir),

    io:fwrite("name_dir: ~p~n", [NameDir]),

    

    InitialState = #state{
        nodes = Nodes,
        name_dir = NameDir
    },

    loop(InitialState),
    io:fwrite("DONE~n", []).



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



