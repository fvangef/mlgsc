.PHONY: clean

test_sources = $(wildcard *.hs)
test_bins = $(basename $(test_sources))
test_scripts =$(wildcard test*.sh)

% :: %.hs
	ghc -i../src --make $<

check: $(test_bins)
	for test in $(test_bins) $(test_scripts); do echo "$$test" ; ./$$test ; done

clean:
	$(RM) $(test_bins) *.o *.hi
