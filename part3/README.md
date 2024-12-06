# Running the project

### If make is  installed

1. Open a terminal in the directory: `part3/`
2. Type `make run` into your terminal
3. Type `Pid = main:spawn_main().` into your terminal

Now the project should be running, use `ctrl+c` to stop the process.

### If  make is not installed

1. Open a terminal in the directory: `part3/`
2. Type `erl -make` into your terminal 
3. Type `erl` into your terminal
4. Type `Pid = main:spawn_main().` into your terminal

Now the project should be running, use `ctrl+c` to stop the process.

# Customizing execution

### Node Id's
- By default the app is going to create nodes from 0 to N-1 (N is defined at the top of main.erl `-define(N, 100).`)
- To manually set Ids comment the line:     `Ids = lists:seq(0, ?N-1),` in the main function and replace it with a list of Ids, Example: `Ids = [1,2,3,50,30,1000,387],`

### Keys
- Keys are stored in keys.csv
- Keys can be added while the program is running by using the command: `Pid ! {add_keys, [1,2,4,31,3,45234,42334,98,1691]}.` (the list of integers can be set to whichever values you want)

### Adding a node manually
- To add a node during execution type in a command such as: `Pid ! {add_node, 30}.`  where 30 is the node that is being added.
- To see the list of all nodes in the DHT ring use the command: `Pid ! print_nodes.`, the command will return a list in this format: `{node,<node_identifier>,<node_number>,<node_pid>}`


### Queries
- Key queries are stored in `key_queries.csv`
- Key queries can be changed during execution and new queries can be made by typing: `Pid ! query_keys.` and all queries from `key_queries.csv` will be added to the CSV outputs.

# Reading outputs
The outputs are CSV files store in `dht_N` where N is the amount of nodes in the DHT circle.

Each node will create two CSV files named `node_number.csv` and `node_number_querries.csv`.

The format of `node_number.csv` is as follows: `node_identifier,successor_identifier,predecessor_identifier|key1_identifier|key2_identifier|key3_identifier..`

Format of `node_number_querries.csv` is as follows: `key_identifier,contacted_node_identifier1|contacted_node_identifier2|contacted_node_identifier3...`