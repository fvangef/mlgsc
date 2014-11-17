.PHONY: clean

targets = mlgsc mlgsc_train mlgsc_xval 

GHCFLAGS = -O2

% :: %.hs
	ghc $(GHCFLAGS) --make $<

all: $(targets)

clean:
	$(RM) $(targets)
