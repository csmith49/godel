GODEL = ../../godel.native
BENCHMARK = eq_ls_stutter
RULES = int_compare base int int_extended string list bool tuple
RULEFILES = $(foreach r, $(RULES), ../../rules/$(r).scm)
RULESTRING = $(foreach r,$(RULEFILES), -rule $(r))

.phony: all
all: consistency.out count

data:
	mkdir data

data/with.out: data
	$(GODEL) -target $(BENCHMARK) $(RULESTRING) -strategy td -dtree -noisy | grep -i "NORMALIZED" | sort  > $@

data/without.out: data
	$(GODEL) -target $(BENCHMARK) $(RULESTRING) -strategy td -dtree -no-kbo -noisy | grep -i "NORMALIZED" | sort > $@

consistency.out: data/with.out data/without.out
	comm -3 data/with.out data/without.out > $@
	wc -l $@

count:
	@echo "WITH: "
	@$(GODEL) -target $(BENCHMARK) $(RULESTRING) -strategy td -dtree -stats | grep -i "NUM_P"
	@echo "WITHOUT: "
	@$(GODEL) -target $(BENCHMARK) $(RULESTRING) -strategy td -dtree -no-kbo -stats | grep -i "NUM_P"

.phony: clean
clean:
	rm -f consistency.out
	rm -rf data/*
