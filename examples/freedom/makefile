.PHONY: all clean

all: freedom.aif

clean:
	git clean -Xdf

%.aif: %.mel
	mel $<

melody/b_hi.aif: melody/b_hi1.aif melody/b_hi1.aif melody/b_hi2.aif melody/b_hi1.aif
	stick $+ $@

melody/a_lo_flanged.aif: melody/a_lo.aif
	flange $^ 0.025

melody/b_lo_flanged.aif: melody/b_lo.aif
	flange $^ 0.0125

parts/a_lo.aif: melody/a_lo.aif melody/a_lo_flanged.aif
	stack $^ $@

parts/b_lo.aif: melody/b_lo.aif melody/b_lo_flanged.aif
	stack $^ $@

parts/chords_a.aif: chords/g.aif chords/g.aif chords/g.aif chords/a.aif
	stick $+ $@

parts/chords_c.aif: chords/e.aif chords/e.aif chords/e.aif chords/a.aif
	stick $+ $@

parts/a1.aif: drums/hihat.aif parts/chords_a.aif
	stack $^ $@

parts/a2.aif: parts/a1.aif drums/bass.aif
	stack $^ $@

parts/a3.aif: parts/a2.aif parts/a_lo.aif
	stack $^ $@

parts/a4.aif: parts/a3.aif melody/a_mi.aif melody/a_hi.aif
	stack $^ $@

parts/b.aif: drums/bass.aif drums/hihat.aif chords/e.aif parts/b_lo.aif melody/b_mi.aif melody/b_hi.aif
	stack $^ $@

parts/c.aif: drums/bass.aif parts/chords_c.aif
	stack $^ $@

parts/intro.aif: parts/a1.aif parts/a2.aif
	stick $+ $@

parts/verse1.aif: parts/a3.aif
	stack $^ $@

parts/verse2.aif: parts/a3.aif
	stack $^ $@

parts/verse3.aif: parts/a3.aif
	stack $^ $@

parts/chorus.aif: parts/a4.aif parts/a4.aif parts/b.aif
	stick $+ $@

parts/outro.aif: parts/c.aif
	stack $^ $@

freedom.aif: parts/intro.aif parts/verse1.aif parts/chorus.aif parts/c.aif parts/verse2.aif parts/chorus.aif parts/c.aif parts/verse3.aif parts/chorus.aif parts/outro.aif
	stick $+ $@
