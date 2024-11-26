-module(dht).
-export([main/1]).
-import(chord_control, [start/1, setup_ring/1, assign_keys/3]).
-import(chord_node, [start/3, handle_message/1]).

main(_) ->
    io:fwrite("...................~n"),


    %% Start the DHT with 10 nodes
    start(10),


    io:fwrite("...................~n"),
    io:fwrite("dht.erl ran successfully!").
