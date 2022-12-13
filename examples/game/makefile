.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: game.wav
mp3: game.mp3

clean:
	git clean -Xdf

%.mp3: %.wav
	ffmpeg -y -i $< $@

%.wav: def.mel %.mel end.mel %.wave
	cat def.mel $*.mel end.mel | mel $*.wave - $@

%.wave: %.txt
	espeak-ng -vmb-de5 -s120 -f$^ -w$@

%.mel: %.gtr
	guitar $< $@

game.wav: parts/part1.wav parts/part2.wav parts/part3.wav parts/part3.wav parts/part4.wav
	stick $+ $@
