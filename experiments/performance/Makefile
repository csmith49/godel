# control the shape of the data
iterations = 10
time_limit = 300
# where's the exec and data and whatnot
data = data
test = python3 generate_data.py
# the benchmarks we want
benchmarks = eq_add eq_max eq_min eq_add_3

.phony: all
all: $(foreach bm,$(benchmarks),$(data)/$(bm).csv)

$(data):
	mkdir -p $(data)

$(data)/%.csv: $(data)
	$(test) $* -i $(iterations) -t $(time_limit) -v -o $@

.phony: clean
clean:
	rm -rf $(data)
