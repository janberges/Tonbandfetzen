.PHONY: wav mp3 clean

wav: harmony.wav
mp3: harmony.mp3

clean:
	git clean -Xdf

%.mp3: %.wav
	ffmpeg -y -i $< $@

%.wav: %.mel
	mel $< $@

harmony.wav: parts/tunings.wav parts/series.wav parts/chords.wav
	stick $+ $@
