![Tonbandfetzen logo](logo/logo.svg)

# Tonbandfetzen

Tonbandfetzen is a collection of command-line tools that allow you to compose
music based on audio fragments generated from plain text input. It contains:

* `mel` - generate sound fragments from plain text
* `guitar` - read guitar tablature
* `stick` - concatenate audio data
* `stack` - superimpose audio data
* `stretch` - rescale audio data
* `repeat` - loop audio data
* `harmonics` - show spectra of wave samples
* `inspect` - show information on audio file
* `riff2aiff` - convert .wav(e) into .aif(f) files
* `aiff2riff` - convert .aif(f) into .wav(e) files

## Usage

Different task are spread across separate executables, which communicate via
files in the Waveform Audio File Format (.wav). Hence, the composition process
can be controlled using build-automation software, allowing for partial updates
and parallel execution (`make -j`).

At the heart of this toolbox, the program `mel` converts text into audio:

    echo "T pyth M A2'8 W ,5 A2' A3' E4' A4' C#v5' E5' Gz5' A5'" | mel | aplay

It partially supports the [Functional Just System](https://misotanni.github.io)
for just intonation invented by misotanni.

## Installation

You will need a recent Fortran compiler. For some features, Python (guitar
tablatures) and eSpeak (example) are required additionally. Using APT, these
prerequisites can be installed as follows:

    sudo apt install gfortran python3 espeak

To compile the binaries, you can use the provided makefile:

    cd /path/to/tonbandfetzen
    make FC=gfortran FFLAGS=-O3

To make the tools and documentation accessible, consider adding the following
lines to your .bashrc:

    REPO=/path/to/tonbandfetzen
    export PATH=$REPO/bin:$PATH
    export PERL5LIB=$REPO/perl/lib:$PERL5LIB
    export MANPATH=$REPO/doc:$MANPATH
    export MANPATH=$REPO/doc/man1:$MANPATH

You might also want to copy the Vim syntax file to the appropriate location:

    ln -s /path/to/tonbandfetzen/vim/mel.vim ~/.vim/syntax/mel.vim

To have Vim detect the corresponding file types, add this line to your .vimrc:

    au BufRead,BufNewFile *.mel,*.gtr set filetype=mel

## Documentation

Please have a look at the examples:

    cd examples/freedom
    make -j 2
    aplay freedom.wav

There is also a man page for each tool:

    man mel
