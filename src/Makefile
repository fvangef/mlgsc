.PHONY: clean install

targets = mlgsc mlgsc_train mlgsc_xval 

GHCFLAGS = -O2
INSTALLDIR = /usr/local/bin

% :: %.hs
	ghc $(GHCFLAGS) --make $<

all: $(targets)

mlgsc: MlgscTypes.hs FastA.hs PepModel.hs NucModel.hs Classifier.hs Align.hs \
	CladeModel.hs CladeModelAux.hs Output.hs \
	OutputFormatStringParser.hs

mlgsc_train: MlgscTypes.hs NewickParser.hs FastA.hs Classifier.hs \
	Weights.hs Alignment.hs CladeModel.hs CladeModelAux.hs

mlgsc_xval: Align.hs Alignment.hs CladeModelAux.hs CladeModel.hs \
	Classifier.hs FastA.hs MlgscTypes.hs MlgscTypes.hs \
	NewickParser.hs PepModel.hs NucModel.hs Weights.hs Output.hs \
	OutputFormatStringParser.hs

install: all
	install $(targets) $(INSTALLDIR)

clean:
	$(RM) $(targets) *.o *.hi
