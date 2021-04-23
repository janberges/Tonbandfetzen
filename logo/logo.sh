#!/bin/bash

a=20
in=2.54

pdflatex --interaction=batchmode logo

convert -density `perl -e "print 640 / ($a / $in)"` logo.pdf \
    -flatten PNG8:logo.png

convert -density `perl -e "print 480 / ($a / $in)"` logo.pdf \
    -background white -gravity center -extent 1280x640 PNG8:logo_banner.png
