# Running the project

### If make is  installed

1. Open a terminal in any of those dirrectories: `part1/`, `part2/`, `part3`
2. Type `make run` into your terminal

Now the project should be running, use `ctrl+c` to stop the process.

### If  make is not installed

1. Open a terminal in any of those dirrectories: `part1/`, `part2/`, `part3`
2. Type `erl -make` into your terminal 
3. Type `escript main.erl` into your terminal

Now the project should be running, use `ctrl+c` to stop the process.

# Customizing execution

### Node Id's
- By default the app is going to create nodes from 0 to N-1 (N is defined at the top of main.erl `-define(N, 100).`)
- To manually set Ids comment the line:     `Ids = lists:seq(0, ?N-1),` in the main function and replace it with a list of Ids, Example: `Ids = [1,2,3,50,30,1000,387],`

### Keys
- Keys are stored in keys.csv
- Key queries are stored in key_queries.csv


# Part 2




# Part 3

For part 3 the makefile starts an erlang script, if the makefile isn't use type in `erl` instead of the escript command

### Commands:
- Start the service:`Pid = main:spawn_main().` 
- Add a node: `Pid ! {add_node, NodeId}.`