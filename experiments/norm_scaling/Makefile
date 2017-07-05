# where we want to store the data
data = data
# the script for making the data
test = python3 generate_data.py --noisy
# where to store the graphs
graphs = graphs
# script for making the graphs
graph = python3 generate_graph.py

# the benchmark we're testing
benchmark = eq_sum_to_first_and_second
bm_data = $(data)/$(benchmark).csv

# the rules
.phony: all
all: $(graphs)
	$(graph) -d $(bm_data)

# make the graphs folder if necessary
$(graphs):
	mkdir -p $(graphs)

# how we get our csv data
$(data)/%.csv: $(data)
	$(test) --benchmark $* --output $@ --width 20

# if we don't have a data folder, make it
$(data):
	mkdir -p $(data)

.phony: clean
clean:
	rm -rf $(data)
