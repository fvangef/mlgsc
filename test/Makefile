.PHONY: clean check_scripts unit_tests

unit_test_srcs = $(wildcard Test[A-Z]*.hs) 
unit_test_progs = $(basename $(unit_test_srcs))
unit_test_srcs := $(filter-out TestFileUtils, $(unit_test_progs))

test_scripts = $(filter-out test_prog.sh, $(wildcard test*.sh))
test_scripts := $(filter-out $(wildcard *func.sh), $(test_scripts))

% :: %.hs
	ghc -i../src --make $<

check: unit_tests check_scripts

unit_tests: $(unit_test_progs)
	@for unittest in $(unit_test_progs); do echo "$$unittest" ; ./$$unittest ; done

check_scripts: $(test_scripts)
	@for test in $(test_scripts); do echo "$$test" ; ./$$test ; done

debug:
	@echo Test scripts: $(test_scripts)
	@echo Unit tests: $(unit_test_progs)

clean:
	$(RM) $(unit_test_progs) *.o *.hi clssfr1.bcls aln1Mod.bcls
