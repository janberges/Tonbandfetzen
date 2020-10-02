compiler = gfortran

ifeq ($(compiler), gfortran)
  options = -std=f2008 -fconvert=big-endian -Wall -pedantic
endif

needless = *.mod .DS_Store

# generated by makemake.py:

programs = harmonics mel reverse stick

.PHONY: all clean cleaner

all: $(programs)

clean:
	@rm -f $(needless) aiff.o constants.o extended.o harmonics.o interpreter.o intervals.o io.o mel.o paths.o rationals.o reverse.o samples.o search.o spectra.o stick.o

cleaner: clean
	@rm -f $(programs)

$(programs):
	@echo link $@
	@$(compiler) -o $@ $^ $(external) $(external_$@)

%.o: %.f90
	@echo compile $*
	@$(compiler) $(options) -c $< -o $@

harmonics: constants.o harmonics.o intervals.o io.o samples.o spectra.o
mel: aiff.o constants.o extended.o interpreter.o intervals.o io.o mel.o paths.o rationals.o samples.o search.o
reverse: aiff.o constants.o extended.o io.o paths.o reverse.o
stick: aiff.o constants.o extended.o io.o stick.o

aiff.o: constants.o extended.o
extended.o: constants.o
harmonics.o: constants.o io.o samples.o spectra.o
interpreter.o: aiff.o constants.o rationals.o samples.o search.o
intervals.o: constants.o
mel.o: aiff.o interpreter.o io.o paths.o
rationals.o: constants.o
reverse.o: aiff.o io.o paths.o
samples.o: constants.o intervals.o
spectra.o: constants.o
stick.o: aiff.o constants.o io.o
