.PHONY: wav mp3 clean
.PRECIOUS: %.wav

wav: freedom.wav
mp3: freedom.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tz tag $<
	ffmpeg -y -i $< $@

%.wav: def.mel %.mel
	cat $^ | tz mel $@

%_fitted.wav: %.wav def.mel %_fit.mel
	cat $(filter-out $<, $^) | tz mel $< - $@

url = https://janberges.de/freedom

%.wav: speak.py %.txt
	wget -O $@ $(url)/$(@F) || curl -o $@ $(url)/$(@F) || python3 $< $*
	touch $@

# The downloaded audio files have been recorded in 2012 using Google Translate,
# GarageBand, and a cable connecting the headphone and microphone jacks.

parts/b_hi.wav: melody/b_hi1.wav melody/b_hi1.wav melody/b_hi2.wav melody/b_hi1.wav
	tz stick $+ $@

parts/chords_a.wav: chords/g.wav chords/g.wav chords/g.wav chords/a.wav
	tz stick $+ $@

parts/chords_c.wav: chords/e.wav chords/e.wav chords/e.wav chords/a.wav
	tz stick $+ $@

parts/a1.wav: drums/hihat.wav parts/chords_a.wav
	tz stack $^ $@

parts/a2.wav: parts/a1.wav drums/bass.wav
	tz stack $^ $@

parts/a3.wav: parts/a2.wav melody/a_lo.wav
	tz stack $^ $@

parts/a4.wav: parts/a3.wav melody/a_mi.wav melody/a_hi.wav
	tz stack $^ $@

parts/b.wav: drums/bass.wav drums/hihat.wav chords/e.wav melody/b_lo.wav melody/b_mi.wav parts/b_hi.wav
	tz stack $^ $@

parts/c.wav: drums/bass.wav parts/chords_c.wav
	tz stack $^ $@

parts/intro.wav: parts/a1.wav parts/a2.wav
	tz stick $+ $@

parts/verse1.wav: parts/a3.wav lyrics/verse1_fitted.wav
	tz stack $^ $@

parts/verse2.wav: parts/a3.wav lyrics/verse2_fitted.wav
	tz stack $^ $@

parts/verse3.wav: parts/a3.wav lyrics/verse3_fitted.wav
	tz stack $^ $@

parts/chorus.wav: parts/a4.wav parts/a4.wav parts/b.wav
	tz stick $+ $@

parts/outro.wav: parts/c.wav lyrics/outro_fitted.wav
	tz stack $^ $@

freedom.wav: parts/intro.wav parts/verse1.wav parts/chorus.wav parts/c.wav parts/verse2.wav parts/chorus.wav parts/c.wav parts/verse3.wav parts/chorus.wav parts/outro.wav
	tz stick $+ $@
