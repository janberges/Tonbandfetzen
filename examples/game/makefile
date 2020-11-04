.PHONY: all clean
.PRECIOUS: %.mel

all: game.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $< $@

%.mel: %.gtr
	guitar $< $@
