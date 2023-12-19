.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: water.wav
mp3: water.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tag $<
	ffmpeg -y -i $< $@

%.wav: %.mel
	mel $< $@

%.mel: %.gtr
	guitar $< $@
