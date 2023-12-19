.PHONY: all clean

all: Tonbandfetzen.pdf Tonbandfetzen.png Tonbandfetzen_banner.png

%.pdf: %.tex
	pdflatex --interaction=batchmode $*

Tonbandfetzen.png: Tonbandfetzen.svg
	inkscape -w 500 -b white -o $@ $<
	python3 optimize.py $@ $@

Tonbandfetzen_banner.png: Tonbandfetzen.png
	convert $< -gravity center -extent 1280x640 $@

clean:
	rm -f *.aux *.log *.pdf *_banner.png
