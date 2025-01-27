.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: liberty.wav
mp3: liberty.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tz tag $<
	ffmpeg -y -i $< $@

%.wav: %.mel
	tz mel $< $@

%.mel: %.gtr
	tz guitar $< $@
