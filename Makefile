build: chord_node.erl chord_control.erl dht.erl
	@erl -compile chord_node.erl chord_control.erl dht.erl
	@echo "dht.erl compiled..."

run: build
	@escript dht.erl
	echo "dht.erl running..."

clean:
	@rm -f *.beam

.PHONY: build run clean
