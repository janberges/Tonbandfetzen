.PHONY: all clean

a = 20
in = 2.54

all: Tonbandfetzen.png Tonbandfetzen_banner.png

%.pdf: %.tex
	pdflatex --interaction=batchmode $*

Tonbandfetzen.png: Tonbandfetzen.pdf
	convert -density `perl -e "print 640 / ($(a) / $(in))"` $< \
        -flatten PNG8:$@

Tonbandfetzen_banner.png: Tonbandfetzen.pdf
	convert -density `perl -e "print 480 / ($(a) / $(in))"` $< \
        -gravity center -extent 1280x640 PNG8:$@

clean:
	rm -f *.aux *.log *.pdf *.png
