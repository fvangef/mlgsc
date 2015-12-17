.PHONY: binaries test clean install

INSTALL_DIR=/usr/local/bin

all: binaries test README.html

binaries:
	cd src && $(MAKE)

test: binaries
	cd test && $(MAKE)

README.html: README.md
	esmd < $< | pandoc -s -o $@

install: 
	install -t $(INSTALL_DIR) src/mlgsc src/mlgsc_train src/mlgsc_xval
	cd man && $(MAKE) install

clean:
	cd src && $(MAKE) clean
	cd test && $(MAKE) clean
	cd man && $(MAKE) clean
