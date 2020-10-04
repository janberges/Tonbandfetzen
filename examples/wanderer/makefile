.PHONY: all clean

all: wanderer.aif

clean:
	git clean -Xdf

%.aif: %.mel
	cd `dirname $<`; mel `basename $<`

%.aif: %.arg
	noise $@ `cat $<`

melody/melody.aif: melody/noise.aif

parts/verse.aif: chords/a.aif chords/b.aif chords/b.aif chords/a.aif
	stick $+ $@

parts/chorus.aif: chords/c.aif melody/melody.aif
	stack $^ $@

wanderer.aif: chords/e.aif parts/verse.aif parts/chorus.aif chords/d.aif chords/e.aif
	stick $+ $@
