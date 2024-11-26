-module(chord_control).
-export([start/1, setup_ring/1, assign_keys/3]).

%% Start the Chord DHT system
start(NodeCount) ->
    DhtPath = io_lib:format("dht_~p", [NodeCount]),
    file:make_dir(DhtPath),
    Nodes = setup_ring(NodeCount),
    io:fwrite("ring generated youpidou!~n"),
    io:fwrite("~p~n", [Nodes]),
    % ring = ["12395","22812","35252","42266","45858","48760","58346","59934","61859","7897","12395"]
    % <============== vérifié jusque là
    % TODO



    Keys = load_keys("keys.csv"),
    assign_keys(Keys, Nodes, DhtPath).

%% Set up nodes and create the ring structure
setup_ring(Count) ->
    %% Create nodes with unique identifiers
    NodeIds = [integer_to_list(erlang:phash2(N, 65536)) || N <- lists:seq(1, Count)],
    %% Sort identifiers to form the ring
    SortedIds = lists:sort(NodeIds),
    lists:append(SortedIds, [hd(SortedIds)]).

%% Assign keys to nodes
assign_keys(Keys, Nodes, DhtPath) ->
    lists:foreach(
        fun(Key) ->
            KeyHash = integer_to_list(erlang:phash2(Key, 65536)),
            %% Find the responsible node for this key
            ResponsibleNode = find_responsible_node(KeyHash, Nodes),
            {chord_node, ResponsibleNode} ! {store_key, KeyHash}
        end,
        Keys),
    %% Log each node's state
    lists:foreach(
        fun({NodeId, _}) -> {chord_node, NodeId} ! {log_node, DhtPath} end,
        Nodes).


%% Auxiliary functions

%% Find the responsible node for a key
find_responsible_node(KeyHash, Nodes) ->
    lists:foldl(
        fun({NodeId, NextNode}, Acc) ->
            case chord_node:is_between(KeyHash, NodeId, NextNode) of
                true -> NodeId;
                false -> Acc
            end
        end, hd(Nodes), Nodes).

%% Load keys from a CSV file
load_keys(FilePath) ->
    {ok, Bin} = file:read_file(FilePath),
    binary:split(Bin, <<"\n">>, [global]).
