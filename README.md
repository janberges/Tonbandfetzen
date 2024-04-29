![Tonbandfetzen logo](logo/Tonbandfetzen.svg)

# Tonbandfetzen

Tonbandfetzen is a collection of command-line tools that allow you to compose
music based on audio fragments generated from plain text input. It contains:

* `mel` - generate sound fragments from plain text
* `guitar` - read guitar tablature
* `stick` - concatenate audio data
* `stack` - superimpose audio data
* `stretch` - rescale audio data
* `repeat` - loop audio data
* `trim` - strip leading and trailing silence
* `mono` - unzip audio channels
* `harmonics` - show spectra of wave samples
* `inspect` - show information on audio file
* `riff2aiff` - convert .wav(e) into .aif(f) files
* `aiff2riff` - convert .aif(f) into .wav(e) files
* `tag` - add ID3v2 metadata to .wav(e) files
* `zplay` - generate and play sound fragments

## Usage

Different task are spread across separate executables, which communicate via
files in the Waveform Audio File Format (.wav). Hence, the composition process
can be controlled using build-automation software, allowing for partial updates
and parallel execution (`make -j`).

At the heart of this toolbox, the program `mel` converts text into audio (try
[here](https://janberges.de/mel.cgi)):

    echo "T pyth M A2'8 W ,5 A2' A3' E4' A4' C#v5' E5' Gz5' A5'" | mel | aplay

The `guitar` preprocessor converts guitar tablature into suitable `mel` input:

    echo "|3 ,30
    E4|--------------------0~~~~~~~~~~~|~~~~0~~~~~~~~~~~~~~~~~~~~~~~~~~~|
    B3|----0~~~~~~~~~~~~~3~~~0~~~~~0~~~|~~3~~~0~~~~~0~~~~~~~~~~~~~~~~~~~|
    G3|--2~~~0~~~~~0~~~~~~~~~~~~~2~~~0~|~~~~~~~~~~2~~~0~~~~~0~~~~~~~~~~~|
    D3|----------2~~~0~~~~~~~~~~~~~~~~~|~~~~~~~~~~~~~~~~~~2~~~0~~~~~0~~~|
    A2|3~~~~~~~~~~~~~~~----------------|--------------------------2~~~0~|
    E2|----------------3~~~~~~~~~~~~~~~|0~~~~~~~~~~~~~~~2~~~~~~~3~~~~~~~|
    " | guitar | mel | repeat | aplay

The [Functional Just System](https://misotanni.github.io) for just intonation
invented by misotanni is partially supported.

## Installation

To build all tools, you only need a recent Fortran compiler:

    cd /path/to/Tonbandfetzen
    make FC=gfortran FFLAGS=-O3

To make the tools and documentation accessible, consider adding the following
lines to your .bashrc:

    REPO=/path/to/Tonbandfetzen
    export PATH=$REPO/bin:$PATH
    export PERL5LIB=$REPO/perl/lib:$PERL5LIB
    export MANPATH=$REPO/doc:$MANPATH

You might also want to link the Vim syntax file to the appropriate location:

    ln -s /path/to/Tonbandfetzen/vim/mel.vim ~/.vim/syntax/mel.vim

To have Vim detect the corresponding file types, add this line to your .vimrc:

    autocmd BufRead,BufNewFile *.mel,*.gtr setlocal filetype=mel

## Documentation

Please have a look at the examples, some of which require Python or eSpeak NG:

    cd examples/freedom
    make -j 2
    aplay freedom.wav

Each program has its own manual page:

    man mel

You can also listen to the examples and browse the manual pages
[here](https://janberges.github.io/Tonbandfetzen).

## Hear also

The name Tonbandfetzen originates from the song Explosion by Tocotronic from
their 2007 album Kapitulation.
