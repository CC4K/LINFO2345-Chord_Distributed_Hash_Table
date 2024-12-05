-module(csv).
-compile(export_all).
-compile([nowarn_export_all, nowarn_unused_record]).
-record(node, {id, non_hashed_id, pid}).
-record(state, {id, non_hashed_id, keys, predecessor, successor, main, fingertable, keys_path, node_count}).
-record(keys_path, {keys_path, keys_left_to_find}).
-record(key_path, {key, path}).

create_csvs(Nodes, NameDir) -> 
    lists:foreach(fun(Node) -> Node#node.pid ! {make_csv, NameDir} end, Nodes),
    io:fwrite("created all csvs~n").
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


create_node_csv(State, NameDir) ->
    NodeId = io_lib:format("~.16B", [State#state.id]),
    PredecessorId = io_lib:format("~.16B", [State#state.predecessor#node.id]),
    SuccessorId = io_lib:format("~.16B", [State#state.successor#node.id]),
    Keys = string:trim(lists:map(fun(Number) -> string:to_lower(io_lib:format("~.16B|", [Number])) end, State#state.keys), trailing, "|"),
    Data = io_lib:format("~s,~s,~s|~s", [string:to_lower(NodeId),  string:to_lower(SuccessorId),string:to_lower(PredecessorId), Keys]),
    FileName = io_lib:format("./~s/~p.csv", [NameDir, State#state.non_hashed_id]),
    {ok, File} = file:open(FileName, [write]),
    file:write(File, Data),
    file:close(File).

create_queries_csv(State, NameDir) ->

    GetLine = fun(Key, NodePath) ->
        NodePathStr = string:join(lists:map(fun(Node) -> io_lib:format("~.16b", [Node#node.id]) end, NodePath), "|"),
        String = io_lib:format("~.16B,~s~n", [Key, NodePathStr]),
        string:to_lower(String)
    end,
    
    KeysPath = State#state.keys_path,
    
    FileLine = lists:map(fun(KeyPath) -> 
        Key = KeyPath#key_path.key,
        Path = KeyPath#key_path.path,
        Line = GetLine(Key, Path),
        Line
    end, KeysPath#keys_path.keys_path),




    % lists:foreach(fun(KeyPath) -> 
    %     Key = KeyPath#key_path.key,
    %     Path = KeyPath#key_path.path,
    %     Line = GetLine(Key, Path),
    %     FileLine = [Line|FileLine]
    % end, KeysPath#keys_path.keys_path),

    Data = string:join(FileLine, ""),
    FileName = io_lib:format("./~s/~p_queries.csv", [NameDir, State#state.non_hashed_id]),
    {ok, File} = file:open(FileName, [write]),
    file:write(File, Data),
    file:close(File).






