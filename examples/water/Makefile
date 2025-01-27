.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: water.wav
mp3: water.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tz tag $<
	ffmpeg -y -i $< $@

%.wav: %.mel
	tz mel $< $@

%.mel: %.gtr
	tz guitar $< $@
