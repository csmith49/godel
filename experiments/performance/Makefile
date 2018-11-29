# control the shape of the data
iterations = 3
time_limit = 300
# where's the exec and data and whatnot
data = data
test = python3 generate_data.py
# the benchmarks we want
benchmarks = eq_add eq_max eq_min eq_add_3 eq_add_4 eq_add_5 \
eq_add_q eq_sub_q eq_mult_q eq_div_q eq_add_c eq_sub_c eq_mult_c \
eq_max_pair eq_intervals eq_min_pair eq_sum_to_first \
eq_sum_to_first_and_second eq_max_and_acc eq_add_and_mult \
eq_distances eq_sum_to_second eq_add_q_long eq_sub_q_long \
eq_mult_q_long eq_div_q_long eq_str_add eq_str_max eq_str_mult \
eq_str_split eq_str_len eq_str_trim_len eq_str_upper_len \
eq_str_lower_len eq_ls_sum eq_ls_min eq_ls_max eq_ls_stutter \
eq_avg_pair eq_ls_sum2 eq_ls_sum_abs

.phony: all
all: $(data) $(foreach bm,$(benchmarks),$(data)/$(bm).csv) graphing

$(data):
	mkdir -p $(data)

$(data)/%.csv:
	$(test) $* -i $(iterations) -t $(time_limit) -v -o $@

graphs:
	mkdir -p graphs

.phony: graphing
graphing: graphs generate_sp.py
	python3 generate_sp.py $(data) -o graphs/bun.pdf
	python3 generate_sp.py $(data) -o graphs/bunk.pdf -k
	python3 generate_sp.py $(data) -o graphs/tdn.pdf -t
	python3 generate_sp.py $(data) -o graphs/tdnk.pdf -t -k

.phony: clean
clean:
	rm -rf $(data)
