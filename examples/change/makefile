.PHONY: all clean

all: change.mp3

clean:
	git clean -Xdf

%.mel: %.py
	python3 $< > $@

%.wav: %.mel
	mel $< $@

%.mp3: %.wav
	ffmpeg -i $< $@
