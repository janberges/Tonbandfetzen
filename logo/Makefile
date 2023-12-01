.PHONY: all clean

a = 20
in = 2.54

all: Tonbandfetzen.png Tonbandfetzen_banner.png

%.pdf: %.tex
	pdflatex --interaction=batchmode $*

Tonbandfetzen.png: Tonbandfetzen.pdf
	convert -density `perl -e "print 399.99 / $(a) * $(in)"` $< -flatten PNG8:$@
	python3 -c "import storylines; storylines.save('$@', storylines.load('$@'))"

Tonbandfetzen_banner.png: Tonbandfetzen.pdf
	convert -density `perl -e "print 479.99 / $(a) * $(in)"` $< \
        -gravity center -extent 1280x640 PNG8:$@

clean:
	rm -f *.aux *.log *.pdf *.png
