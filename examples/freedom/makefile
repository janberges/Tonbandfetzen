.PHONY: all clean

all: freedom.wav

clean:
	git clean -Xdf

%.wav: %.mel
	mel $< $@

%.wav: %.txt
	espeak -vde+f1 -s130 -f$^ -w$@

lyrics/verse1_fitted.wav: lyrics/verse1.wav
	fit $< 16 75 44100

lyrics/verse2_fitted.wav: lyrics/verse2.wav
	fit $< 32 75 44100

lyrics/verse3_fitted.wav: lyrics/verse3.wav
	fit $< 16 75 44100

lyrics/outro_fitted.wav: lyrics/outro.wav
	fit $< 16 75 44100

melody/a_lo_flanged.wav: melody/a_lo.wav
	flange $< 0.025

melody/b_lo_flanged.wav: melody/b_lo.wav
	flange $< 0.0125

parts/a_lo.wav: melody/a_lo.wav melody/a_lo_flanged.wav
	stack $^ $@

parts/b_lo.wav: melody/b_lo.wav melody/b_lo_flanged.wav
	stack $^ $@

parts/b_hi.wav: melody/b_hi1.wav melody/b_hi1.wav melody/b_hi2.wav melody/b_hi1.wav
	stick $+ $@

parts/chords_a.wav: chords/g.wav chords/g.wav chords/g.wav chords/a.wav
	stick $+ $@

parts/chords_c.wav: chords/e.wav chords/e.wav chords/e.wav chords/a.wav
	stick $+ $@

parts/a1.wav: drums/hihat.wav parts/chords_a.wav
	stack $^ $@

parts/a2.wav: parts/a1.wav drums/bass.wav
	stack $^ $@

parts/a3.wav: parts/a2.wav parts/a_lo.wav
	stack $^ $@

parts/a4.wav: parts/a3.wav melody/a_mi.wav melody/a_hi.wav
	stack $^ $@

parts/b.wav: drums/bass.wav drums/hihat.wav chords/e.wav parts/b_lo.wav melody/b_mi.wav parts/b_hi.wav
	stack $^ $@

parts/c.wav: drums/bass.wav parts/chords_c.wav
	stack $^ $@

parts/intro.wav: parts/a1.wav parts/a2.wav
	stick $+ $@

parts/verse1.wav: parts/a3.wav lyrics/verse1_fitted.wav
	stack $^ $@

parts/verse2.wav: parts/a3.wav lyrics/verse2_fitted.wav
	stack $^ $@

parts/verse3.wav: parts/a3.wav lyrics/verse3_fitted.wav
	stack $^ $@

parts/chorus.wav: parts/a4.wav parts/a4.wav parts/b.wav
	stick $+ $@

parts/outro.wav: parts/c.wav lyrics/outro_fitted.wav
	stack $^ $@

freedom.wav: parts/intro.wav parts/verse1.wav parts/chorus.wav parts/c.wav parts/verse2.wav parts/chorus.wav parts/c.wav parts/verse3.wav parts/chorus.wav parts/outro.wav
	stick $+ $@
