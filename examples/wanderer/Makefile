.PHONY: wav mp3 clean

wav: wanderer.wav
mp3: wanderer.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tag $<
	ffmpeg -y -i $< $@

%.wav: %.mel
	mel $< $@

parts/verse.wav: chords/a.wav chords/b.wav chords/b.wav chords/a.wav
	stick $+ $@

parts/chorus.wav: chords/c.wav melody/melody.wav
	stack $^ $@

wanderer.wav: chords/e.wav parts/verse.wav parts/chorus.wav chords/d.wav chords/e.wav
	stick $+ $@
