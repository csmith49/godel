benchmarks = eq_str_len
graph_files = $(foreach bm, $(benchmarks), graphs/$(bm).png) $(foreach bm, $(benchmarks), graphs/$(bm)_td.png)
rules_list = avg base bool int_compare int_extended int list string tuple
rules_dir = ../../rules
rules_files = $(foreach r, $(rules_list), $(rules_dir)/$(r).scm)
cap = 100

.phony: all
all: data $(foreach bm, $(benchmarks), data/$(bm)_bu.csv data/$(bm)_td.csv data/$(bm)_bu_old.csv data/$(bm)_td_old.csv)

data:
	mkdir -p data

data/%_bu.csv:
	python3 generate_data.py $* -i 5 -m $(cap) -r $(rules_files) -o $@ -v

data/%_td.csv:
	python3 generate_data.py $* -i 5 -m $(cap) -r $(rules_files) -o $@ -v -t

data/%_bu_old.csv:
	python3 generate_data.py $* -i 5 -m $(cap) -r $(rules_files) -o $@ -v -s

data/%_td_old.csv:
	python3 generate_data.py $* -i 5 -m $(cap) -r $(rules_files) -o $@ -v -s -t


# from every piece of data we get, it's easy enough to make a graph
graphs/%.png: data/%.csv ../csv_to_graph.py
	python3 ../csv_to_graph.py $< -o $@ --transpose -x "Number of rules" -y "Time (seconds)" -t "$*"

# but we don't want to make it easy to remove the data
.phony: clean
clean:
	rm -rf graphs/*.png
