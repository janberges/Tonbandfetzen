.PHONY: all clean

all: html pdf

clean:
	git clean -Xdf

html: $(patsubst man1/%.1, html/%.html, $(wildcard man1/*.1))
pdf: $(patsubst man1/%.1, html/%.pdf, $(wildcard man1/*.1))

html/%.html: man1/%.1
	groff -mman -Thtml $^ > $@

html/%.pdf: man1/%.1
	groff -mman -Tpdf -fH $^ > $@
