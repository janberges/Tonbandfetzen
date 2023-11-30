.PHONY: wav mp3 clean

wav: spiral.wav
mp3: spiral.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3 ../../logo/Tonbandfetzen.png
	tag $<
	ffmpeg -y -i $< $@

%.png: %.tex
	$(MAKE) -C $(@D)

%.wav: %.mel
	mel $< $@

%.mel: %.py
	python3 $< > $@
