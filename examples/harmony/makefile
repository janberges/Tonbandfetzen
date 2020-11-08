.PHONY: all clean

all: harmony.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $< $@

harmony.wav: parts/tunings.wav parts/series.wav
	stick $+ $@
