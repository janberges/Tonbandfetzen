.PHONY: wav mp3 clean

wav: harmony.wav
mp3: harmony.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3 ../../logo/Tonbandfetzen.png
	tag $<
	ffmpeg -y -i $< $@

%.png: %.tex
	$(MAKE) -C $(@D)

%.wav: %.mel
	mel $< $@

harmony.wav: parts/tunings.wav parts/series.wav parts/chords.wav
	stick $+ $@
