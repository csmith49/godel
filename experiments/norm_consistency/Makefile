GODEL = ../../godel.native
BENCHMARK = eq_str_upper_len
RULES = int_compare base string
RULEFILES = $(foreach r, $(RULES), ../../rules/$(r).scm)

.phony: all
all: consistency.out

data:
	mkdir data

data/dtree.out: data
	$(GODEL) -target $(BENCHMARK) $(foreach r,$(RULEFILES),-rule $(r)) -dtree -noisy | grep -i "NORMALIZED" | sort > $@

data/system.out: data
	$(GODEL) -target $(BENCHMARK) $(foreach r, $(RULEFILES),-rule $(r)) -noisy | grep -i "NORMALIZED" | sort > $@

consistency.out: data/dtree.out data/system.out
	comm -3 data/system.out data/dtree.out > $@
	wc -l $@

.phony: clean
clean:
	rm -f consistency.out
	rm -rf data/*
