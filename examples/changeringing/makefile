.PHONY: all clean

all: changeringing.wav

clean:
	git clean -Xdf

%.mel: %.py
	python3 $< > $@

%.wav: %.mel
	mel $< $@
