examples := $(wildcard */)
examples_wav := $(examples:/=_wav)
examples_mp3 := $(examples:/=_mp3)

.PHONY: test wav mp3 clean $(examples_wav) $(examples_mp3)

test: wav
	md5sum -c examples.md5

wav: $(examples_wav)
mp3: $(examples_mp3)

clean:
	git clean -Xdf

$(examples_wav) $(examples_mp3):
	$(MAKE) -C $(subst _, ,$@)
