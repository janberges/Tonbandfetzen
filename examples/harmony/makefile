.PHONY: all clean

all: harmony.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $< $@
