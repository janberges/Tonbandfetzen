.PHONY: all mp3 clean

all: html pdf mp3 html/example.wav html/example.html

clean:
	git clean -Xdf

html: $(patsubst man1/%.1, html/%.html, $(wildcard man1/*.1))
pdf: $(patsubst man1/%.1, html/%.pdf, $(wildcard man1/*.1))

mp3:
	$(MAKE) -C ../examples/
	rsync -av ../examples/*/*.mp3 html/

html/%.html: man1/%.1
	groff -mman -Thtml $< > $@

html/%.pdf: man1/%.1
	groff -mman -Tpdf -fH $< > $@

%.wav: %.mel
	mel $< $@

%.html: %.mel
	vim -c TOhtml -c "file $@" -c xa! $<
