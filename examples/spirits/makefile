.PHONY: all clean

all: spirits.aif

clean:
	git clean -Xdf

%.aif: %.mel
	mel $<

%.mel: %.gtr
	guitar $< $@

spirits.aif: parts/part1.aif parts/part2.aif parts/part2.aif
	stick $+ $@
