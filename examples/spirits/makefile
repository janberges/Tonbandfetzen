.PHONY: all clean
.PRECIOUS: %.mel

all: spirits.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $< $@

%.mel: %.gtr
	guitar $< $@

spirits.wav: parts/part1.wav parts/part2.wav parts/part2.wav
	stick $+ $@
