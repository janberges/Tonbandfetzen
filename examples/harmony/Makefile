.PHONY: wav mp3 clean

wav: harmony.wav
mp3: harmony.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3 ../../logo/logo.png
	tag $<
	ffmpeg -y -i $< $@

%.png: %.tex
	cd $(@D) && ./logo.sh

%.wav: %.mel
	mel $< $@

harmony.wav: parts/tunings.wav parts/series.wav parts/chords.wav
	stick $+ $@
