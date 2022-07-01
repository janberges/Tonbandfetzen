.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: spirits.wav
mp3: spirits.mp3

clean:
	git clean -Xdf

%.mp3: %.wav
	ffmpeg -y -i $< $@

%.wav: %.mel
	mel $< $@

%.mel: %.gtr
	guitar $< $@
