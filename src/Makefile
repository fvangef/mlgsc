.PHONY: clean

targets = mlgsc mlgsc_train mlgsc_xval 

GHCFLAGS = -O2

% :: %.hs
	ghc $(GHCFLAGS) --make $<

all: $(targets)

mlgsc: MlgscTypes.hs FastA.hs Crumbs.hs NucModel.hs Classifier.hs

mlgsc_train: MlgscTypes.hs NewickParser.hs FastA.hs Classifier.hs \
	Weights.hs Alignment.hs

clean:
	$(RM) $(targets) *.o *.hi
