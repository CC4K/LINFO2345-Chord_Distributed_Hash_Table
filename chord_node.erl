-module(chord_node).
-export([start/3, handle_message/1]).

-record(node, {id, successor, predecessor, keys = []}).

%% Start a new node with ID, successor, and predecessor
start(Id, Successor, Predecessor) ->
    Node = #node{id = Id, successor = Successor, predecessor = Predecessor},
    register({chord_node, Id}, self()),
    handle_message(Node).

%% Loop to handle incoming messages
handle_message(Node) ->
    receive
        {store_key, Key} ->
            NewNode = Node#node{keys = [Key | Node#node.keys]},
            handle_message(NewNode);
        {find_successor, KeyHash, From} ->
            case is_between(KeyHash, Node#node.id, Node#node.successor) of
                true -> From ! {successor, Node#node.successor};
                false -> 
                    %% Forward the request to the successor node
                    {chord_node, Node#node.successor} ! {find_successor, KeyHash, From}
            end,
            handle_message(Node);
        {log_node, DhtPath} ->
            log_node(Node, DhtPath),
            handle_message(Node)
    end.


%% Auxiliary functions

%% Log node details to CSV
log_node(Node, Path) ->
    FileName = filename:join(Path, io_lib:format("node_~s.csv", [Node#node.id])),
    Keys = string:join(Node#node.keys, "|"),
    Data = io_lib:format("~s,~s,~s|~s\n", [Node#node.id, Node#node.successor, Node#node.predecessor, Keys]),
    file:write_file(FileName, Data).

%% Check if a hash lies between two identifiers in the ring
is_between(Key, Start, End) ->
    case (Start < End) of
        true -> (Start =< Key) andalso (Key < End);
        false -> (Start =< Key) orelse (Key < End)
    end.
