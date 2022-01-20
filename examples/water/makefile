.PHONY: wav mp3 clean

wav: water.wav
mp3: water.mp3

clean:
	git clean -Xdf

%.mp3: %.wav
	ffmpeg -y -i $< $@

%.wav: %.mel
	mel $< $@

parts/fsh.wav: bass/fsh.wav melody/verse.wav
	stack $^ $@

parts/b.wav: bass/b.wav melody/verse.wav
	stack $^ $@

parts/csh.wav: bass/csh.wav melody/segue.wav
	stack $^ $@

parts/chords.wav: chords/fsh.wav chords/b.wav chords/csh.wav
	stick $^ $@

parts/melody.wav: parts/fsh.wav parts/fsh.wav parts/fsh.wav parts/fsh.wav parts/b.wav parts/b.wav parts/csh.wav
	stick $+ $@

water.wav: parts/melody.wav parts/chords.wav chords/pattern.wav
	stack $^ $@/
