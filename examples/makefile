examples := $(wildcard */)
examples_wav := $(examples:/=_wav)
examples_mp3 := $(examples:/=_mp3)

.PHONY: wav mp3 clean $(examples_wav) $(examples_mp3)

wav: $(examples_wav)
mp3: $(examples_mp3)

clean:
	git clean -Xdf

$(examples_wav) $(examples_mp3):
	$(MAKE) -C $(subst _, ,$@)
