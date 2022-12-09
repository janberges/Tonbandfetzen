.PHONY: wav mp3 clean
.PRECIOUS: %.wav

wav: freedom.wav
mp3: freedom.mp3

clean:
	git clean -Xdf

%.mp3: %.wav
	ffmpeg -y -i $< $@

%.wav: def.mel %.mel
	cat $^ | mel $@

%_fitted.wav: def.mel %.fit %.wav
	cat $< $*.fit | mel $*.wav - $@

%.wav: %.txt %.speed
	wget -O $@ https://janber.de/freedom/$(@F) || espeak-ng -vmb-de5 -s`cat $*.speed` -f$^ -w$@

# The downloaded audio files have been recorded in 2012 using Google Translate,
# GarageBand, and a cable connecting the headphone and microphone jacks.

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

parts/a3.wav: parts/a2.wav melody/a_lo.wav
	stack $^ $@

parts/a4.wav: parts/a3.wav melody/a_mi.wav melody/a_hi.wav
	stack $^ $@

parts/b.wav: drums/bass.wav drums/hihat.wav chords/e.wav melody/b_lo.wav melody/b_mi.wav parts/b_hi.wav
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
	stick $+ $@/
