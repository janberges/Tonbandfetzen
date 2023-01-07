.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: spirits.wav
mp3: spirits.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3 ../../logo/logo.png
	tag $<
	ffmpeg -y -i $< $@

%.png: %.tex
	cd $(@D) && ./logo.sh

%.wav: %.mel
	mel $< $@

%.mel: %.gtr
	guitar $< $@
