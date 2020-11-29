.PHONY: all clean
.PRECIOUS: %.mel

all: mute.wav

clean:
	git clean -Xdf

%.wav: def.mel %.mel end.mel
	cat $^ | mel $@

%.mel: %.gtr
	guitar $< $@

mute.wav: parts/part1.wav parts/part2.wav parts/part3.wav
	stick $+ $@
