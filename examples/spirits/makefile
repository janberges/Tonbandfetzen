.PHONY: all clean

all: spirits.aif

clean:
	rm -f *.aif

%.aif: %.mel
	mel $<

spirits.aif: parts/part1.aif parts/part2.aif parts/part2.aif
	stick $+ $@
