examples := $(wildcard */)

.PHONY: wav mp3 clean $(examples)

wav: $(examples)
mp3: $(examples)

clean:
	git clean -Xdf

$(examples):
	$(MAKE) -C $@ $(MAKECMDGOALS)
