.PHONY: clean check_scripts

test_sources = $(wildcard *.hs)
test_bins = $(basename $(test_sources))
test_scripts = $(filter-out test_prog.sh, $(wildcard test*.sh))
test_scripts := $(filter-out $(wildcard *func.sh), $(test_scripts))

% :: %.hs
	ghc -i../src --make $<

debug:
	echo $(test_scripts)

check: check_bins check_scripts

check_bins: $(test_bins)
	@for test in $(test_bins) ; do echo "$$test" ; ./$$test ; done

check_scripts: $(test_scripts)
	@for test in $(test_scripts); do echo "$$test" ; ./$$test ; done

clean:
	$(RM) $(test_bins) *.o *.hi
