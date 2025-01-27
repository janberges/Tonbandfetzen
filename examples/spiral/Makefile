.PHONY: wav mp3 clean

wav: spiral.wav
mp3: spiral.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3
	tz tag $<
	ffmpeg -y -i $< $@

%.wav: %.mel
	tz mel $< $@

%.mel: %.py
	python3 $< > $@
