benchmarks = eq_add eq_str_upper_len eq_str_max
graph_files = $(foreach bm, $(benchmarks), graphs/$(bm).png)

.phony: all
all: $(graph_files)

data/eq_add.csv: generate_data.py
	python3 generate_data.py eq_add -i 20 -r ../../rules/int.scm -m 30 -o data/eq_add.csv -v

data/eq_str_upper_len.csv: generate_data.py
	python3 generate_data.py eq_str_upper_len -i 5 -r ../../rules/int_compare.scm ../../rules/string.scm ../../rules/base.scm -m 40 -o data/eq_str_upper_len.csv -v

data/eq_str_max.csv: generate_data.py
	python3 generate_data.py eq_str_max -i 10 -r ../../rules/int_compare.scm ../../rules/string.scm ../../rules/base.scm -m 50 -o data/eq_str_max.csv -v

# from every piece of data we get, it's easy enough to make a graph
graphs/%.png: data/%.csv ../csv_to_graph.py
	python3 ../csv_to_graph.py $< -o $@ --transpose -x "Number of rules" -y "Time (seconds)" -t "$*"

# but we don't want to make it easy to remove the data
.phony: clean
clean:
	rm -rf graphs/*.png
