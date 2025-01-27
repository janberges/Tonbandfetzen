.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: mute.wav
mp3: mute.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tz tag $<
	ffmpeg -y -i $< $@

%.wav: def.mel %.mel end.mel
	cat $^ | tz mel $@

%.mel: %.gtr
	tz guitar $< $@

mute.wav: parts/part1.wav parts/part2.wav parts/part3.wav
	tz stick $+ $@
