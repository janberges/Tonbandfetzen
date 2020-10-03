compiler = gfortran

ifeq ($(compiler), gfortran)
  options = -std=f2008 -fconvert=big-endian -Wall -Wno-maybe-uninitialized -pedantic -Jbuild
endif

needless = *.mod .DS_Store

# generated by makemake.py:

programs = bin/harmonics bin/mel bin/repeat bin/reverse bin/stack bin/stick

.PHONY: all clean cleaner

all: $(programs)

clean:
	@rm -f $(needless) build/aiff.o build/constants.o build/extended.o build/harmonics.o build/interpreter.o build/intervals.o build/io.o build/mel.o build/paths.o build/rationals.o build/repeat.o build/reverse.o build/samples.o build/search.o build/spectra.o build/stack.o build/stick.o

cleaner: clean
	@rm -f $(programs)

$(programs):
	@echo link $@
	@$(compiler) -o $@ $^ $(external) $(external_$@)

build/%.o: src/%.f90
	@echo compile $*
	@$(compiler) $(options) -c $< -o $@

bin/harmonics: build/constants.o build/harmonics.o build/intervals.o build/io.o build/samples.o build/spectra.o
bin/mel: build/aiff.o build/constants.o build/extended.o build/interpreter.o build/intervals.o build/io.o build/mel.o build/paths.o build/rationals.o build/samples.o build/search.o
bin/repeat: build/aiff.o build/constants.o build/extended.o build/io.o build/paths.o build/repeat.o
bin/reverse: build/aiff.o build/constants.o build/extended.o build/io.o build/paths.o build/reverse.o
bin/stack: build/aiff.o build/constants.o build/extended.o build/io.o build/stack.o
bin/stick: build/aiff.o build/constants.o build/extended.o build/io.o build/stick.o

build/aiff.o: build/constants.o build/extended.o
build/extended.o: build/constants.o
build/harmonics.o: build/constants.o build/io.o build/samples.o build/spectra.o
build/interpreter.o: build/aiff.o build/constants.o build/rationals.o build/samples.o build/search.o
build/intervals.o: build/constants.o
build/mel.o: build/aiff.o build/interpreter.o build/io.o build/paths.o
build/rationals.o: build/constants.o
build/repeat.o: build/aiff.o build/io.o build/paths.o
build/reverse.o: build/aiff.o build/io.o build/paths.o
build/samples.o: build/constants.o build/intervals.o
build/spectra.o: build/constants.o
build/stack.o: build/aiff.o build/constants.o build/io.o
build/stick.o: build/aiff.o build/constants.o build/io.o
