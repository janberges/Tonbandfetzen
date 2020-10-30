.PHONY: all clean
.PRECIOUS: %.mel

all: mute.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $< $@

%.mel: %.gtr
	guitar $< $@

mute.wav: parts/part1.wav parts/part2.wav parts/part3.wav
	stick $+ $@
