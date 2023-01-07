.PHONY: wav mp3 clean

wav: change.wav
mp3: change.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3 ../../logo/logo.png
	tag $<
	ffmpeg -y -i $< $@

%.png: %.tex
	cd $(@D) && ./logo.sh

%.wav: %.mel
	mel $< $@

%.mel: %.py
	python3 $< > $@
