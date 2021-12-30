.PHONY: all mp3 clean

all: html pdf mp3

clean:
	git clean -Xdf

html: $(patsubst man1/%.1, html/%.html, $(wildcard man1/*.1))
pdf: $(patsubst man1/%.1, html/%.pdf, $(wildcard man1/*.1))

mp3:
	$(MAKE) -C ../examples/ mp3
	rsync -av ../examples/*/*.mp3 html/

html/%.html: man1/%.1
	pandoc -f man -t html -s -c style.css < $< > $@

html/%.pdf: man1/%.1
	groff -mman -Tpdf -fH $< > $@
