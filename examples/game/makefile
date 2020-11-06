.PHONY: all clean
.PRECIOUS: %.mel

all: game.wav

clean:
	git clean -Xdf

%.wav: def.mel %.mel
	cat $^ | mel $@

%.mel: %.gtr
	guitar $< $@

game.wav: parts/part1.wav parts/part2.wav parts/part3.wav parts/part3.wav parts/part4.wav
	stick $+ $@
