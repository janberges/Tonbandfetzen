.PHONY: all mp3 clean

all: html pdf code mp3

clean:
	git clean -Xdf

html: $(patsubst man1/%.1, html/%.html, $(wildcard man1/*.1))
pdf: $(patsubst man1/%.1, html/%.pdf, $(wildcard man1/*.1))
code: $(patsubst ../examples/%/Makefile, html/%.html, $(wildcard ../examples/*/Makefile))

mp3:
	$(MAKE) -C ../examples/ mp3
	rsync -av ../examples/*/*.mp3 html/

html/%.html: man1/%.1
	pandoc -f man -t html -s -c style.css < $< > $@

html/%.pdf: man1/%.1
	groff -mman -Tpdf -fH $< > $@

.SECONDEXPANSION:
html/%.html: $$(shell git ls-files ../examples/%)
	echo "<html lang='en'><head><meta charset='utf-8'><title>$*</title><link rel='stylesheet' type='text/css' href='style.css'></head><body><h1>$*</h1>" > $@
	for f in $^; do test "$$f" != "$${f#*.git}" && continue; vim $$f -c 'se nonu' -c TOhtml -c 'w! tmp.html' -c 'qa!'; echo "<h2>$${f#../examples/$*/}</h2>" >> $@; awk '/<pre/,/pre>/' tmp.html >> $@; done
	echo "</body></html>" >> $@
	rm -f tmp.html
