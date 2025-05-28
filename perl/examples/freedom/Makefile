.PHONY: all clean
.PRECIOUS: %.wav

all: freedom.aif

clean:
	rm -f *.aif *.wav

%.aif: %.wav %.mel
	tz mel $^ - | tz riff2aiff $@

%.wav: %.txt
	espeak-ng -vde+f5 -s130 -f$< -w$@

freedom.aif: freedom.pl freedom1.aif freedom2.aif freedom3.aif freedom4.aif
	perl $<
