.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: game.wav
mp3: game.mp3

clean:
	git clean -Xdf

%.mp3: %.wav
	ffmpeg -y -i $< $@

%.wav: def.mel %.mel
	cat $^ | mel $@

%.mel: %.gtr
	guitar $< $@

game.wav: parts/part1.wav parts/part2.wav parts/part3.wav parts/part3.wav parts/part4.wav
	stick $+ $@/
