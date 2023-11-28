#!/bin/bash

a=20
in=2.54

pdflatex --interaction=batchmode Tonbandfetzen

convert -density `perl -e "print 640 / ($a / $in)"` Tonbandfetzen.pdf \
    -flatten PNG8:Tonbandfetzen.png

convert -density `perl -e "print 480 / ($a / $in)"` Tonbandfetzen.pdf \
    -background white -gravity center -extent 1280x640 PNG8:Tonbandfetzen_banner.png
