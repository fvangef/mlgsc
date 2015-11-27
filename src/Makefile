.PHONY: clean install

targets = mlgsc mlgsc_train mlgsc_xval mlgsc_dump

GHCFLAGS = --make -O2
INSTALLDIR = /usr/local/bin

% :: %.hs
	ghc $(GHCFLAGS) --make $<

all: $(targets)

mlgsc: mlgsc.hs API.hs MlgscTypes.hs FastA.hs PepModel.hs NucModel.hs \
	Classifier.hs Align.hs PWMModel.hs PWMModelAux.hs Output.hs Trim.hs

mlgsc_train: mlgsc_train.hs API.hs MlgscTypes.hs NewickParser.hs FastA.hs \
	Classifier.hs Weights.hs Alignment.hs PWMModel.hs PWMModelAux.hs \
	IDTree.hs PepModel.hs NucModel.hs

mlgsc_xval: mlgsc_xval.hs API.hs Align.hs Alignment.hs PWMModelAux.hs \
	PWMModel.hs Classifier.hs FastA.hs MlgscTypes.hs MlgscTypes.hs \
	NewickParser.hs PepModel.hs NucModel.hs Weights.hs Output.hs 

mlgsc_dump: mlgsc_dump.hs MlgscTypes.hs NucModel.hs Classifier.hs \
	NewickDumper.hs NucModel.hs PepModel.hs PWMModel.hs

install: all
	install $(targets) $(INSTALLDIR)

clean:
	$(RM) $(targets) *.o *.hi
