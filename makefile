compiler = gfortran

ifeq ($(compiler), gfortran)
  options = -std=f2008 -Wall -Wno-maybe-uninitialized -pedantic -Jbuild
endif

options_aiff = -std=legacy -Wno-pedantic

needless = *.mod .DS_Store

# generated by makemake.py:

programs = bin/aiff2riff bin/fit bin/flange bin/harmonics bin/mel bin/noise bin/repeat bin/reverse bin/riff2aiff bin/stack bin/stick

.PHONY: all clean cleaner

all: $(programs)

clean:
	@rm -f $(needless) build/aiff.o build/aiff2riff.o build/constants.o build/extended.o build/fit.o build/flange.o build/harmonics.o build/interpreter.o build/intervals.o build/io.o build/mel.o build/noise.o build/paths.o build/rationals.o build/repeat.o build/reverse.o build/riff.o build/riff2aiff.o build/samples.o build/search.o build/spectra.o build/stack.o build/stick.o

cleaner: clean
	@rm -f $(programs)

$(programs):
	@echo link $@
	@$(compiler) -o $@ $^ $(external) $(external_$@)

build/%.o: src/%.f90
	@echo compile $*
	@$(compiler) $(options) $(options_$*) -c $< -o $@

bin/aiff2riff: build/aiff.o build/aiff2riff.o build/constants.o build/extended.o build/io.o build/riff.o
bin/fit: build/constants.o build/extended.o build/fit.o build/io.o build/paths.o build/riff.o
bin/flange: build/constants.o build/extended.o build/flange.o build/io.o build/paths.o build/riff.o
bin/harmonics: build/constants.o build/harmonics.o build/intervals.o build/io.o build/samples.o build/spectra.o
bin/mel: build/constants.o build/extended.o build/interpreter.o build/intervals.o build/io.o build/mel.o build/paths.o build/rationals.o build/riff.o build/samples.o build/search.o
bin/noise: build/constants.o build/extended.o build/io.o build/noise.o build/paths.o build/riff.o
bin/repeat: build/constants.o build/extended.o build/io.o build/paths.o build/repeat.o build/riff.o
bin/reverse: build/constants.o build/extended.o build/io.o build/paths.o build/reverse.o build/riff.o
bin/riff2aiff: build/aiff.o build/constants.o build/extended.o build/io.o build/riff.o build/riff2aiff.o
bin/stack: build/constants.o build/extended.o build/io.o build/riff.o build/stack.o
bin/stick: build/constants.o build/extended.o build/io.o build/riff.o build/stick.o

build/aiff.o: build/constants.o build/extended.o
build/aiff2riff.o: build/aiff.o build/constants.o build/io.o build/riff.o
build/extended.o: build/constants.o
build/fit.o: build/constants.o build/io.o build/paths.o build/riff.o
build/flange.o: build/constants.o build/io.o build/paths.o build/riff.o
build/harmonics.o: build/constants.o build/io.o build/samples.o build/spectra.o
build/interpreter.o: build/constants.o build/rationals.o build/riff.o build/samples.o build/search.o
build/intervals.o: build/constants.o
build/mel.o: build/constants.o build/interpreter.o build/io.o build/paths.o build/riff.o
build/noise.o: build/constants.o build/io.o build/paths.o build/riff.o
build/rationals.o: build/constants.o
build/repeat.o: build/constants.o build/io.o build/paths.o build/riff.o
build/reverse.o: build/constants.o build/io.o build/paths.o build/riff.o
build/riff.o: build/constants.o build/extended.o
build/riff2aiff.o: build/aiff.o build/constants.o build/io.o build/riff.o
build/samples.o: build/constants.o build/intervals.o
build/spectra.o: build/constants.o
build/stack.o: build/constants.o build/io.o build/riff.o
build/stick.o: build/constants.o build/io.o build/riff.o
