.PHONY: clean install

targets = mlgsc mlgsc_train mlgsc_xval 

GHCFLAGS = -O2
INSTALLDIR = /usr/local/bin

% :: %.hs
	ghc $(GHCFLAGS) --make $<

all: $(targets)

mlgsc: mlgsc.hs MlgscTypes.hs FastA.hs PepModel.hs NucModel.hs \
	Classifier.hs Align.hs PWMModel.hs PWMModelAux.hs Output.hs

mlgsc_train: mlgsc_train.hs MlgscTypes.hs NewickParser.hs FastA.hs \
	Classifier.hs Weights.hs Alignment.hs PWMModel.hs PWMModelAux.hs

mlgsc_xval: mlgsc_xval.hs Align.hs Alignment.hs PWMModelAux.hs PWMModel.hs \
	Classifier.hs FastA.hs MlgscTypes.hs MlgscTypes.hs \
	NewickParser.hs PepModel.hs NucModel.hs Weights.hs Output.hs 

install: all
	install $(targets) $(INSTALLDIR)

clean:
	$(RM) $(targets) *.o *.hi
