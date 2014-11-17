test_sources = $(wildcard *.hs)
test_bins = $(basename $(test_sources))

% :: %.hs
	ghc -i../src --make $<

check: $(test_bins)
	for test in $(test_bins) ; do echo "$$test" ; ./$$test ; done
