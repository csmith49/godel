benchmarks = eq_add eq_str_upper_len
graph_files = $(foreach bm, $(benchmarks), graphs/$(bm).png)

.phony: all
all: $(graph_files)

data/eq_add.csv: generate_data.py
	python3 generate_data.py eq_add -i 20 -r ../../rules/int.scm -m 30 -o data/eq_add.csv -v

# from every piece of data we get, it's easy enough to make a graph
graphs/%.png: data/%.csv
	python3 ../csv_to_graph.py $< -o $@ --transpose -x "Number of rules" -y "Time (seconds)" -t "$*"

# but we don't want to make it easy to remove the data
.phony: clean
clean:
	rm -rf graphs/*.png
