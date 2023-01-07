.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: mute.wav
mp3: mute.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3 ../../logo/logo.png
	tag $<
	ffmpeg -y -i $< $@

%.png: %.tex
	cd $(@D) && ./logo.sh

%.wav: def.mel %.mel end.mel
	cat $^ | mel $@

%.mel: %.gtr
	guitar $< $@

mute.wav: parts/part1.wav parts/part2.wav parts/part3.wav
	stick $+ $@
