# where we want to store the data
data = data
# the script for making the data
test = python3 generate_data.py --noisy
# the benchmarks we're testing
benchmarks = eq_add_5 eq_sum_to_first_and_second eq_str_split eq_ls_sum2

# the rules
.phony: all
all: $(foreach bm,$(benchmarks),$(data)/$(bm).csv)

# how we get our csv data
$(data)/%.csv: $(data)
	$(test) --benchmark $* --output $@

# if we don't have a data folder, make it
$(data):
	mkdir -p $(data)

.phony: clean
clean:
	rm -rf $(data)
