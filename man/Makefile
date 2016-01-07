.PHONY: clean

MANDIR = /usr/share/man/man1

sources = $(wildcard *.1.md)

manpages = $(addsuffix .1.gz, $(basename $(basename $(sources))))

%.1.gz: %.1.md
	pandoc --standalone --to=man $< | gzip > $@

install: $(manpages)
	install --mode=a+r $(manpages) $(MANDIR)

show:
	@echo $(sources)
	@echo $(manpages)

clean:
	$(RM) $(manpages)
