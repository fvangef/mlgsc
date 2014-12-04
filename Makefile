.PHONY: binaries test clean

all: binaries test

binaries:
	cd src && $(MAKE)

test: binaries
	cd test && $(MAKE)

clean:
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
