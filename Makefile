.PHONY: binaries test clean install

INSTALL_DIR=/usr/local/bin

all: binaries test

binaries:
	cd src && $(MAKE)

test: binaries
	cd test && $(MAKE)

install: 
	install -t $(INSTALL_DIR) src/mlgsc src/mlgsc_train src/mlgsc_xval

README.html: README.md
	pandoc -s -o $@ $<

clean:
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
