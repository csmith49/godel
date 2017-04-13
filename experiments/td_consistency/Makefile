GODEL = ../../godel.native
BENCHMARK = eq_add_4
RULES = int_compare base int int_extended string
RULEFILES = $(foreach r, $(RULES), ../../rules/$(r).scm)
RULESTRING = $(foreach r,$(RULEFILES), -rule $(r))

.phony: all
all: consistency.out

data:
	mkdir data

data/with.out: data
	$(GODEL) -target $(BENCHMARK) $(RULESTRING) -strategy td -dtree -noisy | grep -i "NORMALIZED" | sort  > $@

data/without.out: data
	$(GODEL) -target $(BENCHMARK) $(RULESTRING) -strategy td -dtree -no-kbo -noisy | grep -i "NORMALIZED" | sort > $@

consistency.out: data/with.out data/without.out
	comm -3 data/with.out data/without.out > $@
	wc -l $@

.phony: clean
clean:
	rm -f consistency.out
	rm -rf data/*
