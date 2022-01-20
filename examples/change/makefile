.PHONY: wav mp3 clean

wav: change.wav
mp3: change.mp3

clean:
	git clean -Xdf

%.mp3: %.wav
	ffmpeg -y -i $< $@

%.wav: %.mel
	mel $< $@/

%.mel: %.py
	python3 $< > $@
