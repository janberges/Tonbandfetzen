.PHONY: wav mp3 clean
.PRECIOUS: %.mel

wav: spirits.wav
mp3: spirits.mp3

clean:
	git clean -Xdf

%.mp3: %.wav %.id3 ../../logo/Tonbandfetzen.png
	tag $<
	ffmpeg -y -i $< $@

%.png: %.tex
	$(MAKE) -C $(@D)

%.wav: %.mel
	mel $< $@

%.mel: %.gtr
	guitar $< $@
