.PHONY: binaries test clean install

INSTALL_DIR=/usr/local/bin
BUILD_DIR=.stack-work/install/x86_64-linux/lts-3.13/7.10.2/bin

binaries:
	stack build
	cp $(BUILD_DIR)/mlgsc* src

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
