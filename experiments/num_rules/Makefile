benchmarks = eq_add eq_str_upper_len eq_str_max eq_min_pair
graph_files = $(foreach bm, $(benchmarks), graphs/$(bm).png) $(foreach bm, $(benchmarks), graphs/$(bm)_td.png)
rules_list = avg base bool int_compare int_extended int list string tuple
rules_dir = ../../rules
rules_files = $(foreach r, $(rules_list), $(rules_dir)/$(r).scm)


.phony: all
all: data $(graph_files)

data:
	mkdir -p data

data/%.csv:
	python3 generate_data.py $* -i 5 -m 60 -r $(rules_files) -o $@ -v

data/%_td.csv:
	python3 generate_data.py $* -i 5 -m 60 -r $(rules_files) -o $@ -v -t

# from every piece of data we get, it's easy enough to make a graph
graphs/%.png: data/%.csv ../csv_to_graph.py
	python3 ../csv_to_graph.py $< -o $@ --transpose -x "Number of rules" -y "Time (seconds)" -t "$*"

# but we don't want to make it easy to remove the data
.phony: clean
clean:
	rm -rf graphs/*.png
