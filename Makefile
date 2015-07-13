.PHONY: binaries test clean install

INSTALL_DIR=/usr/local/bin

README.html: README.md
	pandoc -s -o $@ $<

all: binaries test

binaries:
	cd src && $(MAKE)

test: binaries
	cd test && $(MAKE)

install: 
	install -t $(INSTALL_DIR) src/mlgsc src/mlgsc_train src/mlgsc_xval

clean:
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
